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

# Hinzufügen der Variable sustainable.choice
df_tracking <- df_tracking %>%
  left_join(df_long_choice %>% select(participant.code, round.number, sustainable.choice),
            by = c("participant.code", "round.number"))

# Hinzufügen einer neuen Variablen-Kombination aus element_id und attributeType
df_tracking <- df_tracking %>%
  mutate(
    # Extrahiere den Buchstaben (A oder B) aus element_id
    letter = case_when(
      str_detect(element_id, "A") ~ "A",
      str_detect(element_id, "B") ~ "B",
      TRUE ~ NA_character_
    ),
    # Ersetze attributeType durch kurze Namen und entferne Leerzeichen
    attribute_short = case_when(
      attributeType == "Preis" ~ "preis",
      attributeType == "CO2e/ kg" ~ "carbon",   
      attributeType == "Protein" ~ "protein",
      TRUE ~ str_replace_all(attributeType, " ", "") 
    )
  ) %>%
  # Kombiniere die beiden Variablen
  mutate(combined_var = paste0(letter, "_", attribute_short)) %>%
  
  # Entferne die nicht mehr benötigten Variablen
  select(-letter, -attribute_short)

#################################################################################################################################

# Hinzufügen von treatment.group
df_cleaned_unique <- df_long_choice %>%
  select(participant.code, treatment.group) %>%
  distinct()

df_tracking <- df_tracking %>%
  left_join(df_cleaned_unique, by = "participant.code")

df_tracking <- df_tracking %>%
  mutate(treatment.group2 = ifelse(treatment.group %in% c("label", "norm"), "experimental", "control"))

#############################

## Struktur des Datensatzes überprüfen
str(df_tracking)

# Daten exportieren
save(df_tracking, file = "/Users/ausleihe/Desktop/daten/Cleaned_data/df_tracking.Rdata")

