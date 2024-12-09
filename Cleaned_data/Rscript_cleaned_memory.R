library(tidyverse)
library(readr)
library(dplyr)

#################################################################################################################################

# Daten einlesen 
df_long_memory <- read_delim("/Users/ausleihe/data_analysis_master_lara/Raw_data/memory_task_2024-12-08.csv", delim = ",")

#############################

# Nur die ProbandInnen beibehalten, die bis page 90 gekommen sind
participants_reached_90_or_more <- df_long_memory %>%
  filter(participant._index_in_pages >= 90) %>%
  pull(participant.code) 

# Jetzt die Probanden, die weniger als Seite 90 erreicht haben, ausschließen
df_long_memory <- df_long_memory %>%
  filter(participant.code %in% participants_reached_90_or_more)

# Variable participant._index_in_pages entfernen
df_long_memory <- df_long_memory %>%
  select(-participant._index_in_pages)

#############################

# Unnötige Variablen entfernen 
df_long_memory <- df_long_memory %>%
  select(-participant.label,
    -participant._is_bot,
    -participant._max_page_index,
    -participant._current_app_name,
    -participant._current_page_name,
    -participant.time_started_utc,
    -participant.visited,
    -participant.mturk_worker_id,
    -participant.mturk_assignment_id,
    -participant.payoff,
    -player.id_in_group,
    -player.role,
    -player.payoff,
    -group.id_in_subsession,
    -session.code,
    -session.label,
    -session.mturk_HITId,
    -session.mturk_HITGroupId,
    -session.comment,
    -session.is_demo
  )

#############################

# Umwandlung ins Long-Format
df_long_memory <- df_long_memory %>%
  mutate(across(starts_with("player.") | starts_with("subsession."), as.character))

# Aggregiere den Datensatz, sodass jeder Proband nur einmal vorkommt
df_long_memory <- df_long_memory %>%
  group_by(participant.id_in_session, participant.code, subsession.round_number) %>%
  summarise(
    player.AName = first(player.AName),
    player.BName = first(player.BName),
    player.AEstimate = first(player.AEstimate),
    player.BEstimate = first(player.BEstimate),
    player.ACorrect = first(player.ACorrect),
    player.BCorrect = first(player.BCorrect),
    .groups = "drop"
  )

#############################

# Umbenennung der Variablennamen
df_long_memory <- df_long_memory %>%
  rename(
    participant.id = participant.id_in_session,
    name.nonSustainable = player.AName,
    name.Sustainable = player.BName,
    estimate.nonSustainable = player.AEstimate,
    estimate.Sustainable = player.BEstimate,
    correct.nonSustainable = player.ACorrect,
    correct.Sustainable = player.BCorrect,
    round.number = subsession.round_number
  )

#############################

# Konvertiere relevante Spalten in numeric, falls nötig
df_long_memory <- df_long_memory %>%
  mutate(
    correct.nonSustainable = as.numeric(correct.nonSustainable),
    estimate.nonSustainable = as.numeric(estimate.nonSustainable),
    correct.Sustainable = as.numeric(correct.Sustainable),
    estimate.Sustainable = as.numeric(estimate.Sustainable)
  )

# Differenzen zwischen tatsächlichen und geschätzten Memory Task Werten 
df_long_memory <- df_long_memory %>%
  mutate(
    diff_nonSustainable = correct.nonSustainable - estimate.nonSustainable,
    diff_Sustainable = correct.Sustainable - estimate.Sustainable
  )

df_long_memory <- df_long_memory %>%
  mutate(
    # Absolute Differenzen
    absolut_diff = abs(diff_nonSustainable) + abs(diff_Sustainable),
    
    # Differenzen mit Berücksichtigung der Über- und Unterschätzung
    real_diff = diff_nonSustainable + diff_Sustainable
  )

#############################

# Standardisiere `participant.id` und `participant.code` überall
standardize_types <- function(data) {
  data %>%
    mutate(
      participant.id = as.character(participant.id),
      participant.code = as.character(participant.code)
    )
}

df_long_memory <- standardize_types(df_long_memory)
df_wide_choice <- standardize_types(df_wide_choice)
df_long_choice <- standardize_types(df_long_choice)

#############################

# sum_sustainableChoices aus df_wide_choice einfügen
df_long_memory <- df_long_memory %>%
  left_join(df_wide_choice %>% select(participant.code, sum_sustainableChoice), by = "participant.code")

# Berechnung des Durchschnitts der absoluten Differenzen im Memory Task
mean_absolut_diff <- df_long_memory %>%
  group_by(participant.id) %>%
  summarise(mean_absolut_diff = mean(absolut_diff, na.rm = TRUE), .groups = "drop")

#############################

# Füge mean_absolut_diff in df_long_memory ein
df_long_memory <- df_long_memory %>%
  left_join(mean_absolut_diff, by = "participant.id")

# Variable mean_absolut_diff von df_long_memory zu df_long_choice exportieren
df_long_choice <- df_long_choice %>%
  left_join(mean_absolut_diff, by = "participant.id")

# treatment.group hinzufügen aus df_wide_choice
df_long_memory <- df_long_memory %>%
  left_join(df_wide_choice %>% select(participant.code, treatment.group), by = "participant.code")

#############################

# Daten exportieren
save(df_long_memory, file = "/Users/ausleihe/data_analysis_master_lara/Cleaned_data/df_long_memory.Rdata")

