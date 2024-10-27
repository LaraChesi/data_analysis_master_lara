library(tidyverse) 
library(stringr) 

# Daten einlesen 
df_tracking <- read_delim("/Users/ausleihe/Desktop/daten/Raw_data/tracking_demo_2024-10-22.csv", delim = ",") %>% na.omit()
dim(df_tracking)

#############################

# Variablen umbenennen
df_tracking <- df_tracking %>%
  rename(
    participant.code = participant_code,
    round.number = round_number,
    participant.id = id_in_group
  )

#############################

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

#################################################################################################################################

# Hinzufügen von treatment.group2
df_tracking <- df_tracking %>%
  mutate(treatment.group2 = ifelse(treatment.group %in% c("label", "norm"), "experimental", "control"))

#############################

# Daten exportieren
save(df_tracking, file = "/Users/ausleihe/Desktop/daten/Cleaned_data/df_tracking.Rdata")

