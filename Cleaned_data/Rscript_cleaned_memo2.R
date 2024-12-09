library(dplyr)
library(tidyr)

# Daten laden
load("/Users/ausleihe/data_analysis_master_lara/Cleaned_data/df_long_memory.Rdata")

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
  select(-correct_option)

# Schritt 4: Umwandlung der Namen ins Long-Format und sicherstellen, dass `option` übereinstimmt
df_long_memory2 <- df_long_memory2 %>%
  pivot_longer(
    cols = starts_with("name."),
    names_to = "name_option",
    names_prefix = "name.",
    values_to = "name"
  ) %>%
  filter(option == name_option) %>%
  select(-name_option)

# Schritt 5: Nachhaltige Option als Indikatorvariable hinzufügen und Ergebnis bereinigen
df_long_memory2 <- df_long_memory2 %>%
  mutate(sustainable.option = ifelse(option == "Sustainable", 1, 0)) %>%
  select(participant.id, round.number, treatment.group, name, estimate, correct, sustainable.option)

#################################################################################################################################

# Schritt 6: Nachhaltigkeitswerte hinzufügen
df_long_choice_unique <- df_long_choice %>%
  group_by(participant.id) %>%
  summarise(sustainability_score = mean(sustainability_score, na.rm = TRUE), .groups = 'drop')

df_long_memory2 <- df_long_memory2 %>%
  mutate(participant.id = as.character(participant.id))

df_long_choice_unique <- df_long_choice_unique %>%
  mutate(participant.id = as.character(participant.id))

df_tracking <- df_tracking %>%
  mutate(participant.id = as.character(participant.id))

df_long_memory2 <- df_long_memory2 %>%
  left_join(df_long_choice_unique, by = "participant.id") %>%
  distinct()

#################################################################################################################################

# Schritt 7: Lebensmittelzuordnung hinzufügen
food_mapping <- data.frame(
  name = c("Bacon Mac&Cheese", "Kichererbsen-Curry", "Cheeseburger",
           "Falafel-Sandwich", "Spaghetti Meatballs", "Spaghetti Marinara",
           "Beef Jerky", "Trail Mix", "Salami-Pizza", "Chili sin Carne"),
  round.stimuliID_choice = c(7, 7, 8, 8, 11, 11, 14, 14, 9, 9)
)

df_long_memory2 <- df_long_memory2 %>%
  left_join(food_mapping, by = "name")

#################################################################################################################################

# Schritt 8: Relative Differenz berechnen
options(scipen = 999)
df_long_memory2 <- df_long_memory2 %>%
  mutate(relative_diff = abs(estimate - correct) / correct)

#################################################################################################################################

# Schritt 5: df_tracking filtern, um nur relevante combined_var-Werte zu behalten
df_tracking_clean <- df_tracking %>%
  filter(combined_var %in% c("A_co2e/kg", "B_co2e/kg")) %>%
  group_by(participant.id, stimulusID, foodName) %>%
  summarise(combined_var = first(combined_var), .groups = "drop")

# Schritt 6: Join der Daten
df_long_memory2 <- df_long_memory2 %>%
  mutate(participant.id = as.character(participant.id)) %>%
  left_join(
    df_tracking_clean,
    by = c(
      "participant.id" = "participant.id",
      "round.stimuliID_choice" = "stimulusID",
      "name" = "foodName"
    )
  ) %>%
  distinct(participant.id, round.number, name, .keep_all = TRUE)

# carbon_viewed Variable erstellen
df_long_memory2 <- df_long_memory2 %>%
  mutate(carbon_viewed = case_when(
    round.number %in% 1:5 ~ ifelse(!is.na(combined_var), 1, 0),
    round.number %in% 6:7 ~ 0,  
    TRUE ~ NA_real_  
  ))

# prev_seen Variable einfügen
df_long_memory2 <- df_long_memory2 %>%
  mutate(prev_seen = case_when(
    round.number %in% 1:5 ~ 1,
    round.number %in% 6:7 ~ 0,
    TRUE ~ NA_real_
  ))

# sustainable.choice einfügen
df_long_choice_unique <- df_long_choice %>%
  group_by(participant.id) %>%
  summarise(sustainable.choice = first(sustainable.choice), .groups = 'drop') 
df_long_memory2 <- df_long_memory2 %>%
  left_join(select(df_long_choice_unique, participant.id, sustainable.choice), 
            by = "participant.id")

# Correct identifiziert
df_long_memory2 <- df_long_memory2 %>%
  group_by(participant.id, round.number) %>%
  mutate(
    correct_identification = if_else(
      all(estimate[sustainable.option == 1] < estimate[sustainable.option == 0]),
      1,
      0
    )
  ) %>%
  ungroup()

# Hinzufügen von treatment.group2
df_long_memory2 <- df_long_memory2 %>%
  mutate(
    treatment.group2 = case_when(
      treatment.group %in% c("label", "norm") ~ "experimental",
      treatment.group == "control" ~ "control",
      TRUE ~ NA_character_
    )
  )

#################################################################################################################################

# Correct identifiziert
df_long_memory2 <- df_long_memory2 %>%
  group_by(participant.id, round.number) %>%
  mutate(
    correct_identification = if_else(
      length(estimate[sustainable.option == 1]) == 1 &
        length(estimate[sustainable.option == 0]) == 1 &
        estimate[sustainable.option == 1] < estimate[sustainable.option == 0],
      1,
      0
    )
  ) %>%
  ungroup()



#################################################################################################################################

# Daten exportieren
save(df_long_memory2, file = "/Users/ausleihe/data_analysis_master_lara/Cleaned_data/df_long_memory2.Rdata")
