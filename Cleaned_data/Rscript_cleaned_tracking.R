library(tidyverse) 
library(stringr) 

# Daten einlesen 
df_tracking <- read_delim("/Users/ausleihe/data_analysis_master_lara/Raw_data/custom_tracking_demo_2024-12-08.csv", delim = ",")
df_tracking_not.custom <- read_delim("/Users/ausleihe/data_analysis_master_lara/Raw_data/tracking_demo_2024-12-08.csv", delim = ",")

#############################

# Nur die ProbandInnen beibehalten, die bis page 90 gekommen sind
participants_reached_90_or_more <- df_tracking_not.custom %>%
  filter(participant._index_in_pages >= 90) %>%
  pull(participant.code) 

# Jetzt die Probanden, die weniger als Seite 90 erreicht haben, ausschließen
df_tracking_not.custom <- df_tracking_not.custom %>%
  filter(participant.code %in% participants_reached_90_or_more)

#############################

# Variablen umbenennen
df_tracking <- df_tracking %>%
  rename(
    participant.code = participant_code,
    round.number = round_number,
    participant.id = id_in_group
  )

#############################

df_tracking <- df_tracking %>%
  mutate(participant.id = as.character(participant.id))

# Aggregiere df_long_choice nach participant.id
df_long_choice_agg <- df_long_choice %>%
  group_by(participant.id) %>%
  summarise(
    sustainable.choice = first(sustainable.choice),
    treatment.group = first(treatment.group)
  )

# Führe den Join mit der aggregierten Tabelle durch
df_tracking <- df_tracking %>%
  left_join(df_long_choice_agg, by = "participant.id")

# Bereinigen der Kategorien in `attributeType`
df_tracking <- df_tracking %>%
  mutate(
    attributeType = str_to_lower(trimws(attributeType)), 
    attributeType = case_when(
      attributeType %in% c("preis", "price") ~ "price",
      attributeType %in% c("co2e/ kg", "co2e/kg", "co2", "co2e", "co2e/kg") ~ "co2e/kg",
      attributeType %in% c("protein", "eiweiß") ~ "protein",
      TRUE ~ attributeType
    )
  )

#############################

# Hinzufügen einer neuen Variablen-Kombination aus element_id und attributeType
df_tracking <- df_tracking %>%
  mutate(
    # Extrahiere den Buchstaben (A oder B) aus element_id
    letter = case_when(
      str_detect(element_id, "A") ~ "A",
      str_detect(element_id, "B") ~ "B",
      TRUE ~ NA_character_
    ),
    combined_var = paste0(letter, "_", attributeType)
  ) %>%
  # Entferne die nicht mehr benötigten Variablen
  select(-letter)

df_tracking <- df_tracking %>%
  mutate(
    carbon_viewed = case_when(
      combined_var %in% c("A_co2e/kg", "B_co2e/kg") ~ 1,  
      TRUE ~ 0
    )
  )

# Hinzufügen von treatment.group2
df_tracking <- df_tracking %>%
  mutate(treatment.group2 = ifelse(treatment.group %in% c("label", "norm"), "experimental", "control"))

#################################################################################################################################

# Jetzt df_tracking_not.custom und df_tracking zusammenfügen
vars_to_extract <- c(
  "participant.id_in_session",
  "participant.code",
  "player.choice",
  "player.choice_sustainable",
  "player.treatment"
)

df_tracking_not.custom_selected <- df_tracking_not.custom %>%
  select(all_of(vars_to_extract))

# Schritt 2: Sicherstellen, dass beide Datensätze die gleiche `participant.id_in_session` enthalten
# Konvertiere IDs in Zeichen, falls nötig
df_tracking <- df_tracking %>%
  mutate(participant.id = as.character(participant.id))

df_tracking_not.custom_selected <- df_tracking_not.custom_selected %>%
  mutate(participant.id_in_session = as.character(participant.id_in_session))

# Umbenennen der Spalte `participant.id_in_session` in `df_tracking_not.custom_selected`
df_tracking_not.custom_selected <- df_tracking_not.custom_selected %>%
  rename(participant.id = participant.id_in_session)

df_tracking_not.custom_selected <- df_tracking_not.custom_selected %>%
  group_by(participant.id) %>%
  summarise(
    participant.code = first(participant.code),
    player.choice = first(player.choice),
    player.choice_sustainable = first(player.choice_sustainable),
    player.treatment = first(player.treatment)
  )

# Join durchführen
df_tracking <- df_tracking %>%
  left_join(df_tracking_not.custom_selected, by = "participant.id")

#############################

# Daten exportieren
save(df_tracking, file = "/Users/ausleihe/data_analysis_master_lara/Cleaned_data/df_tracking.Rdata")

