library(tidyverse)
library(lme4)
library(emmeans)
library(broom)
library(ggplot2)
library(dplyr)
library(psych)

#################################################################################################################################

# Daten einlesen 
df_wide_choice <- read_delim("/Users/ausleihe/Desktop/daten/Raw_data/all_apps_wide_2024-10-22.csv", delim = ",")

#############################

# Nur die ProbandInnen beibehalten, die bis page 90 gekommen sind
participants_reached_90_or_more <- df_wide_choice %>%
  filter(participant._index_in_pages >= 90) %>%
  pull(participant.code) 

# Jetzt die Probanden, die weniger als Seite 90 erreicht haben, ausschließen
df_wide_choice <- df_wide_choice %>%
  filter(participant.code %in% participants_reached_90_or_more)

# Variable participant._index_in_pages entfernen
df_wide_choice <- df_wide_choice %>%
  select(-participant._index_in_pages)

#############################

# Variablen entfernen
df_wide_choice <- df_wide_choice %>% 
  select(-contains("index"), -contains("memory"), -contains("session.code"), -contains("introduction"), -contains("player.id_in_group"), -contains("session.is_demo"),  -contains("subsession"), -contains("introduction.1.player.id_in_group"), -contains("Check2.y"), -contains("current_page"), -contains("utc"), -contains("max"), -contains("app"), -contains("mturk"), -contains("currency"), -contains("payoff"), -contains("id_in_subsession"), -contains("fee"), -contains("role"), -contains("consent"), -contains("session.label"), -contains("session.comment"), -contains("config"), -contains("visited"), -contains("orderStimuli.y"), -contains("participant.label"), -contains("bot"))

#############################

# Umbenennen der Variablen 
df_wide_choice <- df_wide_choice %>%
  rename(
    participant.id = participant.id_in_session,
    stimuli.order = participant.orderStimuli,
    comp.check1 = participant.comprehensionCheck,
    comp.check2 = participant.comprehensionCheck2,
    frage_1 = NEPR_scale.1.player.frage_1,
    frage_2 = NEPR_scale.1.player.frage_2,
    frage_3 = NEPR_scale.1.player.frage_3,
    frage_4 = NEPR_scale.1.player.frage_4,
    frage_5 = NEPR_scale.1.player.frage_5,
    frage_6 = NEPR_scale.1.player.frage_6,
    frage_7 = NEPR_scale.1.player.frage_7,
    frage_8 = NEPR_scale.1.player.frage_8,
    alter = demographics.1.player.alter,
    geschlecht = demographics.1.player.geschlecht,
    studierende = demographics.1.player.studierende,
    einkommen = demographics.1.player.monatliches_einkommen,
    einkauf = demographics.1.player.haushalts_einkauf,
    politische.O = demographics.1.player.politische_orientierung,
    ernährung = demographics.1.player.ernaehrungsgewohnheiten,
    ernährung_other= demographics.1.player.ernaehrungsgewohnheiten_other,
    email = demographics.1.player.email,
    treatment.group = tracking_demo.1.player.treatment
  )

# Behalte nur die erste tracking_demo.1.player.treatment-Variable
df_wide_choice <- df_wide_choice %>%
  select(-matches("tracking_demo\\.(2|3|4|5|6|7|8|9|10|11|12|13|14|15)\\.player\\.treatment"))  

# Umbenennen der choice_sustainable Variablen
for (i in 1:15) {
  df_wide_choice <- df_wide_choice %>%
    rename(!!paste0("round", i, "_choice.sustainable") := !!sym(paste0("tracking_demo.", i, ".player.choice_sustainable")))
}

# Umbenennen der choice Variablen
for (i in 1:15) {
  df_wide_choice <- df_wide_choice %>%
    rename(!!paste0("round", i, "_choice") := !!sym(paste0("tracking_demo.", i, ".player.choice")))
}

# Umbenennen der sustainableLeft Variablen
for (i in 1:15) {
  df_wide_choice <- df_wide_choice %>%
    rename(!!paste0("round", i, "_sustainableLeft") := !!sym(paste0("tracking_demo.", i, ".player.sustainableLeft")))
}

################################################################################################################################# 

## Ordinaldaten numerisch umwandeln

# Geschlecht
df_wide_choice$num_geschlecht <- (df_wide_choice$geschlecht == "Weiblich") * 1 + 
  (df_wide_choice$geschlecht == "Männlich") * 2 +
  (df_wide_choice$geschlecht == "Divers") * 3 +
  (df_wide_choice$geschlecht == "Sonstige") * 4

# Studierende
df_wide_choice$num_studierende <- (df_wide_choice$studierende == "Ja") * 1 + 
  (df_wide_choice$studierende == "Nein") * 2

# Haushaltseinkauf
df_wide_choice$num_einkauf <- (df_wide_choice$einkauf == "Ja") * 1 + 
  (df_wide_choice$einkauf == "Nein") * 2

# Einkommen
df_wide_choice$num_einkommen <- (df_wide_choice$einkommen == "0-1000") * 1 + 
  (df_wide_choice$einkommen == "1001-3000") * 2 +
  (df_wide_choice$einkommen == "3001-5000") * 3 +
  (df_wide_choice$einkommen == "5001-7000") * 4 +
  (df_wide_choice$einkommen == "Mehr als 7000") * 5 +
  (df_wide_choice$einkommen == "Ich möchte keine Angabe machen") * 6
df_wide_choice$num_einkommen

# Politische Orientierung
df_wide_choice$num_politische_orientierung <- (df_wide_choice$politische.O == "Links") * 1 + 
  (df_wide_choice$politische.O == "Mitte-links") * 2 +
  (df_wide_choice$politische.O == "Mitte") * 3 +
  (df_wide_choice$politische.O == "Mitte-rechts") * 4 +
  (df_wide_choice$politische.O == "Rechts") * 5 +
  (df_wide_choice$politische.O == "Ich möchte keine Angabe machen") * 6

# Ernährungsgewohnheiten
df_wide_choice$num_ernährung <- (df_wide_choice$ernährung == "AllesesserIn") * 1 + 
  (df_wide_choice$ernährung == "Vegetarisch") * 2 +
  (df_wide_choice$ernährung == "Vegan") * 3

# Comprehension Check 
df_wide_choice <- df_wide_choice %>% 
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

# Sustainability Score
# Berechnung der Scores für die drei Fragebögen
df_wide_choice <- df_wide_choice %>%
  mutate(
    score_fragebogen_1 = rowMeans(select(., frage_1:frage_4), na.rm = TRUE),
    score_fragebogen_2 = rowMeans(select(., frage_5:frage_6), na.rm = TRUE),
    score_fragebogen_3 = rowMeans(select(., frage_7:frage_8), na.rm = TRUE)
  )

# Korrelationen zwischen den Ergebnissen der drei Fragebögen
cor_matrix <- cor(df_wide_choice %>% select(score_fragebogen_1, score_fragebogen_2, score_fragebogen_3), use = "complete.obs")
print(cor_matrix)
# Moderate positive Korrelation zwischen den Fragebögen 

# Konsistenzprüfung
alpha_result <- alpha(df_wide_choice %>% select(score_fragebogen_1, score_fragebogen_2, score_fragebogen_3))
print(alpha_result)
# akzeptable bis gute interne Konsistenz für die aggregierten Scores der drei Fragebögen

# Aggregation der Scores
df_wide_choice <- df_wide_choice %>%
  mutate(sustainability_score = rowMeans(select(., score_fragebogen_1:score_fragebogen_3), na.rm = TRUE))

#################################################################################################################################

# Zwei statt drei treatment Gruppen
df_wide_choice <- df_wide_choice %>%
  mutate(treatment.group2 = ifelse(treatment.group %in% c("label", "norm"), "experimental", "control"))

# Variable sum_sustainableChoice hinzufügen
# Erstelle eine Liste der Variablen, die die nachhaltigen Entscheidungen enthalten
sustainable_vars <- paste0("round", 1:15, "_choice.sustainable")

# Füge eine neue Variable hinzu, die die Anzahl der nachhaltigen Entscheidungen (Wert 1) zählt
df_wide_choice <- df_wide_choice %>%
  mutate(sum_sustainableChoice = rowSums(select(., all_of(sustainable_vars)) == 1, na.rm = TRUE))

# Überprüfe das Ergebnis
head(df_wide_choice$sum_sustainableChoice)

################################################################################################################################# 

# Daten exportieren
save(df_wide_choice, file = "/Users/ausleihe/data_analysis_master_lara/Cleaned_data/df_wide_choice.Rdata")

