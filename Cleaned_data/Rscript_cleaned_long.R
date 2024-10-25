library(tidyverse)
library(lme4)
library(emmeans)
library(broom)
library(ggplot2)
library(dplyr)

#################################################################################################################################

# Daten einlesen 
df_long_choice <- read_delim("/Users/ausleihe/Desktop/daten/Raw_data/all_apps_wide_2024-10-22.csv", delim = ",")

#############################

# Nur die ProbandInnen beibehalten, die bis page 90 gekommen sind
participants_reached_90_or_more <- df_long_choice %>%
  filter(participant._index_in_pages >= 90) %>%
  pull(participant.code) 

# Jetzt die Probanden, die weniger als Seite 90 erreicht haben, ausschließen
df_long_choice <- df_long_choice %>%
  filter(participant.code %in% participants_reached_90_or_more)

# Variable participant._index_in_pages entfernen
df_long_choice <- df_long_choice %>%
  select(-participant._index_in_pages)

#############################

# Unnötige Variablen entfernen
df_long_choice <- df_long_choice %>% 
  select(-contains("NEPR"), -contains("demographics"), -contains("index"), -contains("memory"), -contains("session.code"), -contains("introduction"), -contains("player.id_in_group"), -contains("session.is_demo"),  -contains("subsession"), -contains("introduction.1.player.id_in_group"), -contains("Check2.y"), -contains("current_page"), -contains("utc"), -contains("max"), -contains("app"), -contains("mturk"), -contains("currency"), -contains("payoff"), -contains("id_in_subsession"), -contains("fee"), -contains("role"), -contains("consent"), -contains("session.label"), -contains("session.comment"), -contains("config"), -contains("visited"), -contains("orderStimuli.y"), -contains("participant.label"), -contains("bot"))

#############################

# In Long-Format umwandeln
df_long_choice <- df_long_choice %>%
  mutate(across(starts_with("tracking_demo.") | starts_with("memory_task.") | 
                  starts_with("NEPR_scale.") | starts_with("demographics.") | 
                  starts_with("num_"), as.character))

df_long_choice <- df_long_choice %>%
  pivot_longer(
    cols = starts_with("tracking_demo.") | 
      starts_with("memory_task.") | 
      starts_with("NEPR_scale.") | 
      starts_with("demographics.") | 
      starts_with("num_"),  
    names_to = c("round", ".value"),
    names_pattern = "(\\d+)\\.(.*)"
  )

#############################

# Entfernen von NA-Werten in 'player.treatment'
df_long_choice <- df_long_choice %>%
  filter(!is.na(player.treatment))

#################################################################################################################################

# Umwandlung von 'round' in numerisch
df_long_choice <- df_long_choice %>%
  mutate(round = as.numeric(round))

# Umwandlung der Stimuli-Reihenfolge und Erstellen neuer Variablen
df_long_choice <- df_long_choice %>%
  mutate(participant.orderStimuli = gsub("\\[|\\]", "", participant.orderStimuli)) %>%
  mutate(stimuli_order = strsplit(participant.orderStimuli, ", ")) %>%
  mutate(stimuli_order = lapply(stimuli_order, function(x) as.numeric(trimws(x)))) %>%
  group_by(participant.code) %>%
  mutate(
    round = round - 1,  
    roundstimuliID = sapply(round, function(i) {
      order_list <- stimuli_order[[1]]
      round_index <- i  
      if (round_index >= 0 && round_index < length(order_list)) {
        return(order_list[round_index + 1])
      } else {
        return(NA)
      }
    })
  ) %>%
  ungroup() %>%
  select(-stimuli_order)

#################################################################################################################################

# Umbenennung der Variablennamen
df_long_choice <- df_long_choice %>%
  rename(
    participant.id = participant.id_in_session,
    stimuli.order = participant.orderStimuli,
    comp.check1 = participant.comprehensionCheck,
    comp.check2 = participant.comprehensionCheck2,
    round.number = round,
    choice = player.choice,
    sustainable.left = player.sustainableLeft,
    treatment.group = player.treatment,
    sustainable.choice = player.choice_sustainable,
    round.stimuliID= roundstimuliID
  )

#################################################################################################################################

# Ordinaldaten in numerische umwandeln
# Comprehension Check 
df_long_choice <- df_long_choice %>% 
  mutate(
    comp.check1 = case_when(
      comp.check1 == "correct" ~ 1,
      comp.check1 %in% c("a_false", "b_false", "c_false") ~ 0,
      TRUE ~ NA_real_
    ),
    comp.check2 = case_when(
      comp.check2 == "correct" ~ 1,
      comp.check2 %in% c("a_false", "b_false", "c_false") ~ 0,
      TRUE ~ NA_real_
    )
  )

################################################################################################################################# 

# DataFrame aus der Liste der Stimuli mit Preis, Emissionen und Proteine

stimuli_list <- data.frame(
  StimulusID = 0:14,
  PriceA = c(3, 1.25, 7, 1.15, 0.36, 0.51, 16, 16.5, 25, 25.50, 20, 19.50, 17.50, 1.95, 29.50),
  CO2A = c(5718, 2187, 3802, 5763, 4400, 3780, 5351, 21693, 4419, 16363, 17914, 18357, 11094, 23019, 20825),
  ProteinA = c(20, 7, 4, 4, 1, 2, 32, 36, 33, 47, 35, 44, 32, 10, 35),
  PriceB = c(1.60, 1.18, 7, 1, 0.49, 0.81, 17.50, 14.50, 22, 12.50, 17.50, 17.50, 19, 0.70, 28),
  CO2B = c(1116, 622, 2334, 1498, 900, 1155, 1479, 1478, 1215, 1384, 2100, 1918, 1575, 2422, 895),
  ProteinB = c(9, 10, 0.8, 10, 1, 3, 18, 23, 29, 23, 20, 25, 24, 4, 27),
  NameA = c("Eier mit Speck", "Schoko-Cerealien mit Kuhmilch", "Schokoladeneis", "Vollmilchschokolade", 
            "Mango", "Joghurt-Müsliriegel", "Bacon Mac&Cheese", "Cheeseburger", "Salami-Pizza", 
            "Steak", "Spaghetti Meatballs", "Lasagne mit Hackfleisch", "Roastbeef-Sandwich", 
            "Beef Jerky", "Lammkotelett"),
  NameB = c("Hummus mit Brot", "Haferflocken mit pflanzlicher Milch und Früchten", "Früchtesorbet", 
            "Erdnussbutter-Marmeladen-Sandwich", "Apfel", "Cracker", "Kichererbsen-Curry", 
            "Falafel-Sandwich", "Chili sin Carne", "Gemüseauflauf", "Spaghetti Marinara", 
            "Vegetarische Spaghetti Bolognese", "Veganer Burrito", "Trail Mix", "Makrele")
)

df_long_choice <- df_long_choice %>%
  left_join(stimuli_list, by = c("round.stimuliID" = "StimulusID"))

# Erstellen neuer Variablen basierend auf der Wahl des Teilnehmers und der Stimulus Reihenfolge
df_long_choice <- df_long_choice %>%
  mutate(
    price.diff = PriceB - PriceA,
    protein.diff = ProteinB - ProteinA,
    CO2.diff = CO2B - CO2A,
    food.chosen = ifelse(sustainable.choice == 1, NameB, NameA)
  )

#################################################################################################################################

# Zwei statt drei treatment Gruppen 
df_long_choice <- df_long_choice %>%
  mutate(treatment.group2 = ifelse(treatment.group %in% c("label", "norm"), "experimental", "control"))
df_long_choice$treatment.group2 <- as.factor(df_long_choice$treatment.group2)

# Variable mean_sbsolut_diff hinzufügen
df_long_choice <- df_long_choice %>%
  left_join(mean_absolut_diff %>% select(participant.id, mean_absolut_diff), 
            by = "participant.id")  

# Variable sustainability_score hinzufügen
df_long_choice <- merge(df_long_choice, 
                        df_wide_choice[, c("participant.code", "sustainability_score")], 
                        by = "participant.code", 
                        all.x = TRUE)

################################################################################################################################# 

# In numeric umwandeln
df_long_choice <- df_long_choice %>%
  mutate(
    price.diff = as.numeric(gsub(",", ".", price.diff)),
    protein.diff = as.numeric(gsub(",", ".", protein.diff)),
    CO2.diff = as.numeric(as.character(CO2.diff)),
    participant.id = as.numeric(as.character(participant.id)),
    sustainable.choice = as.numeric(sustainable.choice),
    mean_absolut_diff = as.numeric(as.character(mean_absolut_diff)),
    sustainability_score = as.numeric(as.character(sustainability_score))
  )

################################################################################################################################# 

# Daten exportieren
save(df_long_choice, file = "/Users/ausleihe/Desktop/daten/Cleaned_data/df_long_choice.Rdata")

