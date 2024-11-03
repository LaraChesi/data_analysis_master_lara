
library(dplyr)
library(tidyr)

load("/Users/ausleihe/Desktop/daten/Cleaned_data/df_long_memory.Rdata")

#################################################################################################################################

# Schritt 1: Wähle die relevanten Spalten
df_long_memory2 <- df_long_memory %>%
  select(participant.id, round.number, treatment.group,
         estimate.nonSustainable, estimate.Sustainable,
         correct.nonSustainable, correct.Sustainable,
         name.nonSustainable, name.Sustainable)

# Schritt 2: Umwandlung der Schätzungen ins Long-Format
df_long_memory2 <- df_long_memory2 %>%
  pivot_longer(
    cols = starts_with("estimate."),
    names_to = "option",
    names_prefix = "estimate.",
    values_to = "estimate"
  )

# Schritt 3: Umwandlung der Korrektheits-Spalten ins Long-Format und sicherstellen, dass `option` übereinstimmt
df_long_memory2 <- df_long_memory2 %>%
  pivot_longer(
    cols = starts_with("correct."),
    names_to = "correct_option",
    names_prefix = "correct.",
    values_to = "correct"
  ) %>%
  filter(option == correct_option) %>%
  select(-correct_option)  # Entferne die temporäre Spalte

# Schritt 4: Umwandlung der Namen ins Long-Format und sicherstellen, dass `option` übereinstimmt
df_long_memory2 <- df_long_memory2 %>%
  pivot_longer(
    cols = starts_with("name."),
    names_to = "name_option",
    names_prefix = "name.",
    values_to = "name"
  ) %>%
  filter(option == name_option) %>%
  select(-name_option)  # Entferne die temporäre Spalte

# Schritt 5: Nachhaltige Option als Indikatorvariable hinzufügen und Ergebnis bereinigen
df_long_memory2 <- df_long_memory2 %>%
  mutate(sustainable.option = ifelse(option == "Sustainable", 1, 0)) %>%
  select(participant.id, round.number, treatment.group, name, estimate, correct, sustainable.option)

# Überprüfen der Struktur des neuen DataFrames
glimpse(df_long_memory2)

#################################################################################################################################

# Schritt 2: Einzigartige Einträge in df_long_choice zusammenfassen
df_long_choice_unique <- df_long_choice %>%
  group_by(participant.id) %>%
  summarise(sustainability_score = mean(sustainability_score, na.rm = TRUE), .groups = 'drop')

# Schritt 3: Mit den Nachhaltigkeitswerten aus dem einzigartigen df_long_choice zusammenführen
df_long_memory2 <- df_long_memory2 %>%
  mutate(participant.id = as.character(participant.id))

df_long_choice_unique <- df_long_choice_unique %>%
  mutate(participant.id = as.character(participant.id))

df_long_memory2 <- df_long_memory2 %>%
  left_join(df_long_choice_unique, by = "participant.id") %>%
  
  # Duplikate entfernen
  distinct()

#################################################################################################################################

# Mapping der Lebensmitteloptionen definieren
food_mapping <- data.frame(
  name = c("Bacon Mac&Cheese", "Kichererbsen-Curry", "Cheeseburger",  
           "Falafel-Sandwich", "Spaghetti Meatballs", "Spaghetti Marinara",  
           "Beef Jerky", "Trail Mix", "Salami-Pizza", "Chili sin Carne"),
  round.stimuliID_choice = c(7, 7, 8, 8, 11, 11, 14, 14, 9, 9)
)

# Die Zuordnung mit dem ursprünglichen DataFrame anhand der 'name'-Variablen zusammenführen
df_long_memory2 <- df_long_memory2 %>%
  left_join(food_mapping, by = "name")

# Den aktualisierten DataFrame anzeigen
head(df_long_memory2)

#############################

# Berechnung der relativen Differenz 
options(scipen = 999)
df_long_memory2 <- df_long_memory2 %>%
  mutate(relative_diff = abs(estimate - correct) / correct)
  
#############################

# Schritt 5: df_tracking filtern, um nur relevante combined_var-Werte zu behalten
df_filtered_tracking <- df_tracking %>%
  filter(combined_var %in% c("A_co2e/kg", "B_co2e/kg"))

df_long_memory2 <- df_long_memory2 %>%
  mutate(participant.id = as.numeric(participant.id))

# Schritt 6: Join der Daten
df_long_memory2 <- df_long_memory2 %>%
  left_join(df_filtered_tracking %>% 
              select(participant.id, stimulusID, foodName, combined_var),
            by = c("participant.id" = "participant.id", 
                   "round.stimuliID_choice" = "stimulusID",
                   "name" = "foodName"))

#############################

# carbon_viewed Variable erstellen
df_long_memory2 <- df_long_memory2 %>%
  mutate(carbon_viewed = case_when(
    round.number %in% 1:5 ~ ifelse(!is.na(combined_var), 1, 0),
    round.number %in% 6:7 ~ 0,  
    TRUE ~ NA_real_  
  ))

# sustainable.choice einfügen
df_long_choice_unique <- df_long_choice %>%
  group_by(participant.id) %>%
  summarise(sustainable.choice = first(sustainable.choice), .groups = 'drop') # oder median, mode, etc.
df_long_memory2 <- df_long_memory2 %>%
  left_join(select(df_long_choice_unique, participant.id, sustainable.choice), 
            by = "participant.id")

#################################################################################################################################

# Daten exportieren
save(df_long_memory2, file = "/Users/ausleihe/data_analysis_master_lara/Cleaned_data/df_long_memory2.Rdata")

