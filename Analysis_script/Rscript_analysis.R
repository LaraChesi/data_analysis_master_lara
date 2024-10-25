library(readr)
library(dplyr)
library(ggplot2)
library(psych)
library(lme4)
library(emmeans)
library(corrplot)
library(car)
library(tidyr)
library(stringr)
library(tibble)

# DATEN EINLESEN
load("/Users/ausleihe/Desktop/daten/Cleaned_data/df_long_choice.Rdata")
load("/Users/ausleihe/Desktop/daten/Cleaned_data/df_wide_choice.Rdata")
load("/Users/ausleihe/Desktop/daten/Cleaned_data/df_long_memory.Rdata")
load("/Users/ausleihe/Desktop/daten/Cleaned_data/df_tracking.Rdata")

#################################################################################################################################

# Anzahl Probanden 
count_probanden <- function(df) {
  n_distinct(df_long_choice$participant.code)
}

# Probandenanzahl in jedem Datensatz ermitteln
anzahl_probanden_long_choice <- count_probanden(df_long_choice)
anzahl_probanden_wide_choice <- count_probanden(df_wide_choice)
anzahl_probanden_memory <- count_probanden(df_memory)
anzahl_probanden_tracking <- count_probanden(df_tracking)

# Ergebnisse ausgeben
cat("Anzahl der Probanden in df_long_choice:", anzahl_probanden_long_choice, "\n")
cat("Anzahl der Probanden in df_wide_choice:", anzahl_probanden_wide_choice, "\n")
cat("Anzahl der Probanden in df_memory:", anzahl_probanden_memory, "\n")
# Alle 421
cat("Anzahl der Probanden in df_tracking:", anzahl_probanden_tracking, "\n")
# 407

#################################################################################################################################

# DESKRIPTIVE STATISTIK

psych::describe(df_wide_choice)

# Deskriptive Statistik für Alter
summary(df_wide_choice$alter) 
sd_age <- sd(df_wide_choice$alter, na.rm = TRUE)
sd_age

# Histogramm der Altersverteilung
ggplot(df_wide_choice, aes(x = alter)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Altersverteilung", x = "Alter", y = "Häufigkeit") +
  theme_minimal()

# Boxplot der Altersverteilung
ggplot(df_wide_choice, aes(y = alter)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "Boxplot der Altersverteilung", y = "Alter") +
  theme_minimal()

# Häufigkeiten und Prozente für Geschlecht
gender_freq <- table(df_wide_choice$geschlecht)
gender_perc <- prop.table(gender_freq) * 100
print(gender_freq)
print(gender_perc)

# Häufigkeiten und Prozente für Ernährungsgewohnheiten
diet_freq <- table(df_wide_choice$ernährung)
diet_perc <- prop.table(diet_freq) * 100
print(diet_freq)
print(diet_perc)

# Häufigkeiten und Prozentsätze für Ernährungsgewohnheiten pro Behandlungsgruppe
diet_summary <- df_wide_choice %>%
  group_by(treatment.group2, ernährung) %>%
  summarise(count = n(), .groups = 'drop') %>% 
  group_by(treatment.group2) %>%
  mutate(percentage = (count / sum(count)) * 100) %>%
  arrange(treatment.group2, ernährung)
print(diet_summary)

# Häufigkeiten und Prozente für Politische Orientierung
politics_freq <- table(df_wide_choice$politische.O)
politics_perc <- prop.table(politics_freq) * 100
print(politics_freq)
print(politics_perc)

# Häufigkeiten und Prozente für Studierende
studis_freq <- table(df_wide_choice$studierende)
studis_perc <- prop.table(studis_freq) * 100
print(studis_freq)
print(studis_perc)

# Häufigkeiten und Prozente für Haushaltseinkauf
einkauf_freq <- table(df_wide_choice$einkauf)
einkauf_perc <- prop.table(einkauf_freq) * 100
print(einkauf_freq)
print(einkauf_perc)

# Häufigkeiten und Prozente für Einkommen
income_freq <- table(df_wide_choice$einkommen)
income_perc <- prop.table(income_freq) * 100
print(income_freq)
print(income_perc)

# Häufigkeitstabellen 
table(df_wide_choice$geschlecht, df_wide_choice$ernährung)
table(df_wide_choice$geschlecht, df_wide_choice$politische.O)

# Summe der nachhaltigen Entscheidungen pro politischer Gruppe
sustainable_per_politicalGroup <- aggregate(sum_sustainableChoice ~ politische.O, data = df_wide_choice, FUN = sum)
print(sustainable_per_politicalGroup)

# Count der Teilnehmer pro politischer Gruppe
count_per_group <- aggregate(participant.code ~ politische.O, data = df_wide_choice, FUN = length)

# Prozentuale Verteilung der nachhaltigen Entscheidungen pro politischer Gruppe
percent_sustainable_per_group <- merge(sustainable_per_politicalGroup, count_per_group, by = "politische.O")
percent_sustainable_per_group$percent_sustainable <- (percent_sustainable_per_group$sum_sustainableChoice / (15 * percent_sustainable_per_group$participant.code)) * 100
print(percent_sustainable_per_group)

# Summe der nachhaltigen Entscheidungen pro Geschlecht
sustainable_per_gender <- aggregate(sum_sustainableChoice ~ geschlecht, data = df_wide_choice, FUN = sum)
print(sustainable_per_gender)

# Count der Teilnehmer pro Geschlecht
count_per_gender <- aggregate(participant.code ~ geschlecht, data = df_wide_choice, FUN = length)

# Prozentuale Verteilung der nachhaltigen Entscheidungen pro Geschlecht
percent_sustainable_per_gender <- merge(sustainable_per_gender, count_per_gender, by = "geschlecht")
percent_sustainable_per_gender$percent_sustainable <- (percent_sustainable_per_gender$sum_sustainableChoice / (15 * percent_sustainable_per_gender$participant.code)) * 100
print(percent_sustainable_per_gender)

# Boxplot erstellen df_tracking
boxplot(df_tracking$duration)
# Hier die drei Ausreisser rausschmeissen oder drinnen lassen?

# Q-Q-Plot erstellen df_tracking
qqnorm(df_tracking$duration, main = "Q-Q-Plot der Dauer")
qqline(df_tracking$duration, col = "red")

# Wahl nachhaltiger vs. nicht-nachhaltiger Optionen
choice_counts <- df_long_choice %>%
  group_by(sustainable.choice) %>%
  summarise(count = n())
print(choice_counts)

# Nicht-Nachhaltig: 1949
# Nachhaltig: 4366

# Anzahl nachhaltiger und nicht nachhaltiger Entscheidungen pro Gruppe
choice_counts_by_group <- df_long_choice %>%
  group_by(treatment.group2, sustainable.choice) %>%
  summarise(count = n()) %>%
  mutate(choice_type = ifelse(sustainable.choice == 1, "Nachhaltig", "Nicht Nachhaltig"))
print(choice_counts_by_group)

# Prozentsätze nachhaltiger und nicht nachhaltiger Entscheidungen pro Gruppe
choice_percentages_by_group <- df_long_choice %>%
  group_by(treatment.group2) %>%
  summarise(
    count_sustainable = sum(sustainable.choice == 1),
    count_unsustainable = sum(sustainable.choice == 0),
    total_count = n(),
    nachhaltig = (count_sustainable / total_count) * 100,
    nicht_nachhaltig = (count_unsustainable / total_count) * 100,
    .groups = 'drop'
  ) %>%
  select(treatment.group2, nachhaltig,  nicht_nachhaltig)
print(choice_percentages_by_group)
#                                 Nachhaltig            Nicht-Nachhaltig
# control                           67.7                     32.3
# experimental                      69.8                     30.2

# Mittelwerte und Standardabweichungen
df_long_choice %>% 
  group_by(treatment.group2) %>%
  summarise(
    mean_sustainable_choice = mean(sustainable.choice, na.rm = TRUE),
    sd_sustainable_choice = sd(sustainable.choice, na.rm = TRUE),
    sd_recall_accuracy = sd(absolut_diff, na.rm = TRUE)
  )

# Häufigkeiten
table(df_long_choice$treatment.group2)
table(df_long_choice$sustainable.choice)

#################################################################################################################################

# GRAFISCHE DARSTELLUNG 

# Balkendiagramm für Einkommen
income_freq <- df_wide_choice %>%
  count(einkommen) %>%
  mutate(percentage = n / sum(n) * 100)

ggplot(income_freq, aes(x = einkommen, y = n, fill = einkommen)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Häufigkeit des monatlichen Einkommens", x = "Monatliches Einkommen", y = "Häufigkeit") + 
  theme_minimal() + 
  scale_fill_brewer(palette = "Set6") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 200))

# Boxplot für Einkommen nach Geschlecht
ggplot(df_wide_choice, aes(x = factor(num_geschlecht), y = num_einkommen)) +
  geom_boxplot() +
  labs(title = "Einkommen nach Geschlecht", x = "Geschlecht", y = "Einkommen")

# Korrelation abhängig von der Ernährung
model <- lm(num_ernährung ~ num_geschlecht + num_studierende + num_einkauf + num_politische_orientierung + num_einkommen, data = df_wide_choice)
summary(model)
# Insbesondere zeigen Frauen und Menschen mit linker politischer Orientierung umweltfreundlichere Ernährungsgewohnheiten im Vergleich zu Männern und Menschen mit rechter politischer Orientierung. 
# Ein höheres Einkommen ist ebenfalls mit weniger umweltfreundlichen Ernährungsgewohnheiten assoziiert.

# Korrelationsmatrix für alle Daten (numerisch)
cor_matrix <- cor(df_wide_choice %>% select(num_ernährung, num_einkommen, num_geschlecht, num_studierende, num_einkauf, num_politische_orientierung), use = "complete.obs")
print(cor_matrix)

# Korrelation-Plot erstellen für alle Daten (numerisch)
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.cex = 0.7)

#################################################################################################################################

# H1-H3: LOGISTISCHE MULTILEVEL-REGRESSION

# Modell 1: Logistische Regression, um die Wahrscheinlichkeit nachhaltiger Entscheidungen über die Zeit zu untersuchen
# Mit "round" als Prädiktor und "participant" las random intercept
mod1 <- glmer(sustainable.choice ~ round.number + (1 | participant.id) + (1 | round.stimuliID), 
              data = df_long_choice, family = binomial)
summary(mod1)
# Signifikante Unterschiede zwischen Teilnehmenden und Tendenz, nachhaltige Entscheidungen zu treffen
# kein signifikanter Einfluss der Runden auf nachhaltige Wahl 

#############################

# Modell 2: Erweiterung von Modell 1 um die Gruppenzugehörigkeit 
# Control, Label, Norm als festen Effekt, um zu prüfen, ob die Gruppenzugehörigkeit einen signifikanten Einfluss auf die Wahl hat
mod2 <- glmer(sustainable.choice ~ round.number + treatment.group2 + (1 | participant.id) + (1 | round.stimuliID), 
              data = df_long_choice, family = binomial)
summary(mod2)
# Experimentelle Behandlungsansatz hat keinen signifikanten Einfluss auf die nachhaltige Wahl
# Effekt der Runde allein ist nicht signifikant

#############################

# Modell 3: Hinzufügen der Pro-Environmental Attitudes und CO2-Emissionen als Kontrollvariablen
# Z-Standardisierung
df_long_choice <- df_long_choice %>%
  mutate(across(c(round.number, sustainability_score, CO2.diff), ~ scale(.)[ ,1]))

mod3 <- glmer(sustainable.choice ~ round.number + treatment.group2 + 
                sustainability_score + CO2.diff +
                (1 | participant.id) + (1 | round.stimuliID), 
              data = df_long_choice, family = binomial)
summary(mod3)
# participant.id: signifikante Unterschiede zwischen den Teilnehmern
# round.stimuliID: Unterschiede zwischen den Stimuli 
# höhere Nachhaltigkeitswerte korrelieren mit einer höheren Wahrscheinlichkeit für nachhaltige Entscheidungen

#############################

# Korrelationen zwischen Variablen prüfen
cor_matrix <- cor(df_long_choice %>% select(price.diff, CO2.diff, protein.diff, sustainability_score), use = "complete.obs")
print(cor_matrix)

#################################################################################################################################

# MODERATORANALYSE

# Modell 4: Interaktion zwischen Gruppenzugehörigkeit und Pro-Environmental Attitudes
# Untersucht, ob die Pro-Umwelt-Einstellungen den Einfluss der Gruppenzugehörigkeit auf die Entscheidung zur Nachhaltigkeit moderieren
mod4 <- glmer(sustainable.choice ~ treatment.group2 * sustainability_score + 
                price.diff + CO2.diff + protein.diff + 
                (1 | participant.id) + (1 | round.stimuliID), 
              data = df_long_choice, 
              family = binomial, 
              control = glmerControl(optimizer = "bobyqa"))
summary(mod4)
# sustainability_score signifikant: ein höherer Nachhaltigkeitswert ist mit einer höheren Wahrscheinlichkeit verbunden, dass eine nachhaltige Wahl getroffen wird
# Alle anderen Variablen sind nicht signifikant, Interaktion auch nicht 

# Visualisierung
ggplot(df_long_choice, aes(x = sustainability_score, y = sustainable.choice, color = treatment.group)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Interaktion zwischen Gruppenzugehörigkeit und Pro-Umwelt-Einstellungen", x = "Pro-Umwelt-Einstellungen", y = "Nachhaltige Entscheidung") +
  theme_minimal() +
  scale_color_manual(values = c("control" = "red", "label" = "blue", "norm" = "green"))

#############################

# Modell 5: Kontrastanalyse
# Unterschiede zwischen den Gruppen (Interventionsgruppen vs. Kontrollgruppe und Label vs. Norm) hinsichtlich ihrer Umwelteinstellungen und deren Einfluss auf nachhaltige Entscheidungen
emm <- emmeans(mod4, ~ treatment.group2 | sustainability_score)
contrast(emm, method = "pairwise", by = "sustainability_score", adjust = "bonferroni")

# Visualisierung der Kontraste
# Wahrscheinlichkeit, dass Teilnehmer eine nachhaltige Wahl treffen, in Abhängigkeit von der Behandlungsgruppe (Kontrollgruppe vs. Experimentalgruppe)
emm_df <- as.data.frame(emm)
emm_df$probability <- plogis(emm_df$emmean)  # Wahrscheinlichkeit

# Konfidenzintervalle berechnen
emm_df$lower_ci <- plogis(emm_df$emmean - (1.96 * emm_df$SE))  # Unteres CI
emm_df$upper_ci <- plogis(emm_df$emmean + (1.96 * emm_df$SE))  # Oberes CI

# Plot erstellen
ggplot(emm_df, aes(x = treatment.group2, y = probability, color = treatment.group2)) +
  geom_point(size = 3) +  # Punkte anzeigen
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +  # Konfidenzintervalle
  labs(title = "Wahrscheinlichkeit der nachhaltigen Wahl nach Behandlungsgruppe",
       x = "Behandlungsgruppe",
       y = "Wahrscheinlichkeit der nachhaltigen Wahl") +
  theme_minimal()

#################################################################################################################################

# H4: MULTILEVEL REGRESSION ANALYSIS: RECALL ACCURACY
# Hinzufügen der Variable, ob die Kohlenstoffattribute, die beim Memory-Test abgefragt werden, angesehen wurden
# Nur von den relevanten round.stimuliID, die im memory_task abgefragt werden: 7, 8, 14, 9, 11
interested_stimuli <- c(7, 8, 14, 9, 11)

# Trimme führende und nachfolgende Leerzeichen in der attributeType-Spalte
df_tracking <- df_tracking %>%
  mutate(attributeType = trimws(attributeType))

# Hinzufügen der Variable, ob das Kohlenstoffattribut in den interessierenden Runden angesehen wurde
df_trackingCO2 <- df_tracking %>%
  group_by(participant.id) %>%
  summarise(
    carbon_viewed = as.integer(any(attributeType == "CO2e/ kg" & stimulusID %in% interested_stimuli)),
    .groups = 'drop'
  )

# Umwandlung der participant.id in einen Faktor
df_trackingCO2 <- df_trackingCO2 %>%
  mutate(participant.id = as.factor(participant.id))

# Umwandlung der participant.id in einen Faktor in df_long_choice
df_long_choice <- df_long_choice %>%
  mutate(participant.id = as.factor(participant.id))

# Merge df_trackingCO2 mit df_long_choice
df_long_choice <- df_long_choice %>%
  left_join(df_trackingCO2, by = "participant.id")

# Visualisierung 
ggplot(df_long_choice, aes(x = carbon_viewed, y = mean_absolut_diff, color = treatment.group)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Recall-Accuracy je nach Gruppenzugehörigkeit und CO2-Ansicht", x = "CO2-Attribut angesehen (0 = Nein, 1 = Ja)", y = "Recall-Accuracy") +
  theme_minimal() +
  scale_color_manual(values = c("control" = "red", "label" = "blue", "norm" = "green"))

#############################





# DAS FUNKTIONIERT NOCH NICHT 



# Multilevel-Regression für Recall Accuracy abhängig davon, ob das Carbon Attribut gesehen wurde sowie von den Umwelt-Werten
model_recall <- lmer(mean_absolut_diff ~ treatment.group + carbon_viewed + sustainability_score +
                     (1 | participant.id) + (1 | round.stimuliID), 
                   data = df_long_choice)
summary(model_recall)

# Interaktioneffekte: Gruppenzugehörigkeit und Ansehen des Carbon Attributs auf Recall Accuracy 
mod_interaction_recall <- lmer(mean_absolut_diff ~ treatment.group * carbon_viewed + 
                                 (1 | participant.id) + (1 | round.stimuliID), 
                               data = df_long_choice)
summary(mod_interaction_recall)

# Kontraste zur Untersuchung der Gruppenunterschiede
emm <- emmeans(mod_recall_interaction, ~ treatment.group | sustainability_score)
contrast(emm)

# Visualisierung der Recall-Accuracy
ggplot(df, aes(x = sustainability_score, y = mean_absolut_diff, color = treatment.group)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Recall-Accuracy vs. Umwelt-Einstellungen", x = "Pro-Umwelt-Einstellungen", y = "Recall-Genauigkeit") +
  theme_minimal()

#################################################################################################################################

# Weitere Plots zur Visualisierung der Daten
ggcol3 <- scale_color_manual(values = c("red", "blue", "green"))  

ggplot(df_long_choice, aes(round.number, sustainable.choice, na.rm = TRUE)) + 
  stat_smooth(aes(group = participant.id, color = treatment.group), method = "lm", formula = y ~ x, se = FALSE, na.rm = TRUE, size = 0.1) + 
  stat_smooth(aes(group = treatment.group, color = treatment.group), method = "lm", formula = y ~ x, se = FALSE, na.rm = TRUE, size = 1.5, linetype = "solid") + 
  ggcol3 + ylab("choice") + xlab("rounds")

ggplot(df_long_choice, aes(round.number, sustainable.choice, na.rm = TRUE)) + 
  stat_smooth(aes(group = participant.id, color = treatment.group), method = "loess", formula = y ~ x, se = FALSE, span = 1, na.rm = TRUE, size = 0.1) + 
  stat_smooth(aes(group = treatment.group, color = treatment.group), method = "loess", formula = y ~ x, span = 1, se = FALSE, na.rm = TRUE, size = 1.5, linetype = "solid") + 
  ggcol3 + facet_wrap(~treatment.group) + ylab("choice") + xlab("rounds") 

ggplot(data = df_long_choice[as.numeric(substr(df_long_choice$participant.id, 1, 5)) %% 10 == 0,], aes(x = round.number, y = sustainable.choice, group = participant.id)) + 
  stat_summary(aes(color = treatment.group), fun = mean, geom = "point", size = 0.5) + 
  stat_smooth(aes(color = treatment.group), method = "loess", formula = y ~ x, span = 1, se = FALSE, na.rm = TRUE, size = 0.3, linetype = "dashed") + 
  stat_smooth(aes(color = treatment.group), method = "lm", formula = y ~ x, se = FALSE, na.rm = TRUE, size = 0.3) + 
  facet_wrap(~participant.id) + ggcol3 + ylab("choice") + xlab("rounds") + ylim(0, 1)

#################################################################################################################################

# T-TESTS /ANOVA

# Vergleich der Anzahl nachhaltiger Entscheidungen zwischen den beiden experimentellen Gruppen und der Kontrollgruppe
# Daten korrekt bereinigen und vorbereiten
control <- df_long_choice %>% filter(treatment.group == "control")
label <- df_long_choice %>% filter(treatment.group == "label")
norm <- df_long_choice %>% filter(treatment.group == "norm")

#############################

# Anzahl der nachhaltigen Entscheidungen pro Gruppe
sustainable_counts <- df_long_choice %>%
  group_by(treatment.group) %>%
  summarise(sustainable_choices = sum(sustainable.choice))
print(sustainable_counts)

# Boxplot zur Visualisierung der Verteilung nachhaltiger Entscheidungen in den drei Gruppen
ggplot(df_long_choice, aes(x = treatment.group, y = sustainable.choice, fill = treatment.group)) +
  geom_bar(stat = "identity") +
  labs(title = "Anzahl nachhaltiger Entscheidungen pro Gruppe",
       x = "Gruppe",
       y = "Anzahl nachhaltiger Entscheidungen") 

# T-Test zwischen control und label
controllabel <- t.test(control$sustainable.choice, label$sustainable.choice)
print(controllabel)
# Unterschied zwischen den Mittelwerten der nachhaltigen Entscheidungen in der Kontroll- und Labelgruppe statistisch nicht signifikant

# T-Test zwischen control und norm
controlnorm <- t.test(control$sustainable.choice, norm$sustainable.choice)
print(controlnorm)
# Unterschied zwischen den Mittelwerten der nachhaltigen Entscheidungen in der Kontroll- und Normgruppe statistisch nicht signifikant

# ANOVA für nachhaltige Entscheidungen zwischen den Gruppen
anova_model <- aov(sustainable.choice ~ treatment.group, data = df_long_choice)
anova_summary <- summary(anova_model)
print(anova_summary)
# Kein signifikanter Unterschied 

# ANOVA für Recall Accuracy
anova_recall_accuracy <- aov(mean_absolut_diff ~ treatment.group2, data = df_long_choice)
summary(anova_recall_accuracy)
# Äusserst Signifikanter Unterschied in der Recall Accuracy zwischen den Behandlungsgruppen

# ANOVA für Pro-Environmental Attitudes
anova_pro_env <- aov(sustainability_score ~ treatment.group, data = df_long_choice)
summary(anova_pro_env)
# Sehr signifikanter Unterschied zwischen den Mittelwerten der Nachhaltigkeitswerte (sustainability_score) in den verschiedenen Behandlungsgruppen (treatment.group)

#################################################################################################################################

# SEKUNDÄRE ANALYSEN 

# 1. Recall Performance
# Analyse der Recall-Performance in Abhängigkeit von den Umwelteinstellungen
model_recall2 <- lmer(mean_absolut_diff ~ sustainability_score + treatment.group + (1 | participant.id), data = df_long_choice)
summary(model_recall2)

# 2. Impact of Pro-environmental Attitudes
# Vergleich der Recall-Leistung basierend auf den Umweltwerten
model_pro_env <- lmer(mean_absolut_diff ~ sustainability_score + (1 | participant.id), data = df)
summary(model_pro_env)


# BIS HIER OBEN DIE ERSTEN BEIDEN FUNKTIONIEREN NOCH NICHT 





# Visualisierung der Recall-Leistung in Abhängigkeit von den Umweltwerten
ggplot(df_long_choice, aes(x = sustainability_score, y = mean_absolut_diff)) +
  geom_point(aes(color = treatment.group)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Recall Performance vs. Pro-Environmental Attitudes", 
       x = "Pro-Environmental Attitudes", 
       y = "Recall Performance (Absolute Difference)") +
  theme_minimal()

# 3. Effect of Interventions
# Analyse der Auswirkungen der Interventionen auf die Betrachtung von Umweltattributen
tracking_data_intervention <- df_long_choice %>%
  group_by(treatment.group) %>%
  summarise(carbon_viewed_mean = mean(carbon_viewed, na.rm = TRUE), .groups = 'drop')

# Visualisierung der Betrachtung von Kohlenstoffattributen
ggplot(tracking_data_intervention, aes(x = treatment.group, y = carbon_viewed_mean)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Durchschnittlicher Carbon View nach Behandlungsgruppe",
       x = "Behandlungsgruppe",
       y = "Durchschnittlicher Carbon View") +
  theme_minimal()

#############################

# Erstellen von first_attribute_viewed
first_viewed <- df_tracking %>%
  filter(view_order == 1) %>%
  select(participant.id, round.number, combined_var) %>%
  rename(first_attribute_viewed = combined_var)

# Erstellen von last_attribute_viewed
last_viewed <- df_tracking %>%
  group_by(participant.id, round.number) %>%
  filter(view_order == max(view_order)) %>%
  ungroup() %>%
  select(participant.id, round.number, combined_var) %>%
  rename(last_attribute_viewed = combined_var)

first_viewed$participant.id <- as.character(first_viewed$participant.id)
last_viewed$participant.id <- as.character(last_viewed$participant.id)

# Mergen der neuen Variablen mit dem Hauptdatensatz
df_long_choice <- df_long_choice %>%
  left_join(first_viewed, by = c("participant.id", "round.number")) %>%
  left_join(last_viewed, by = c("participant.id", "round.number"))

# 4. Predictive Relationships
# Analyse, ob Umweltbedenken das Suchverhalten vorhersagen können
model_search_behavior <- glmer(sustainable.choice ~ sustainability_score + carbon_viewed + (1 | participant.id), data = df_long_choice, family = binomial)
summary(model_search_behavior)
# positive und signifikante Beziehung zwischen dem sustainability_score und der Wahrscheinlichkeit, eine nachhaltige Wahl zu treffen, zeigt, dass Teilnehmer mit höheren Bewertungen in Bezug auf Nachhaltigkeit eher nachhaltige Entscheidungen treffen
# Schätzwert für carbon_viewed ist positiv, aber nicht signifikant

#############################






# Das hier geht noch nicht





# Explorative Analyse: Suchen von Mustern in der Informationssuche und deren Einfluss auf Essensentscheidungen
model_choice_patterns <- glmer(sustainable.choice ~ first_attribute_viewed + last_attribute_viewed + (1 | participant.id), 
                               data = df_long_choice, 
                               family = binomial)

# 5. Robustness Check
# Exklusion der Teilnehmer, die den Verständnischeck beim ersten Versuch bestanden haben
df_excluded <- df_long_choice %>% filter(comp.check1== 0)  

# Wiederholung der Analysen nach Ausschluss
model_recall_excluded <- lmer(mean_absolut_diff ~ sustainability_score + treatment.group + (1 | participant.id), data = df_excluded)
summary(model_recall_excluded)

model_pro_env_excluded <- lmer(mean_absolut_diff ~ sustainability_score + (1 | participant.id), data = df_excluded)
summary(model_pro_env_excluded)

model_search_behavior_excluded <- glm(sustainable.choice ~ sustainability_score + carbon_viewed + (1 | participant.id), data = df_excluded, family = binomial)
summary(model_search_behavior_excluded)

model_choice_patterns_excluded <- glm(sustainable.choice ~ first_attribute_viewed + last_attribute_viewed + (1 | participant.id), data = df_excluded, family = binomial)
summary(model_choice_patterns_excluded)

#################################################################################################################################

# PRÜFUNG DER VORAUSSETZUNGEN FÜR DIE MODELLE - noch nicht fertig

# Multikollinearität (VIF) für alle Prädiktoren im Modell
vif(mod3)
print(vif_results)

#############################

# Hierarchische Struktur und Intraklassenkorrelation (ICC mit einem Nullmodell)

# Nullmodell erstellen
null_model <- glmer(sustainable.choice ~ 1 + (1 | participant.id), data = df, family = binomial)

# Vollständiges Modell erstellen
full_model <- glmer(sustainable.choice ~ round.number + treatment.group + (1 | participant.id), data = df, family = binomial)

# Extrahiere Varianz der Zufallseffekte aus dem Nullmodell
var_null_model <- as.data.frame(VarCorr(null_model))$vcov[1]

# Extrahiere Varianz der Zufallseffekte und Residuenvarianz aus dem vollständigen Modell
var_full_model_random <- as.data.frame(VarCorr(full_model))$vcov[1]  
var_full_model_residual <- sigma(full_model)^2 

# Berechne ICC
icc <- var_full_model_random / (var_full_model_random + var_full_model_residual)
print(icc)  # 0.4788337
cat("48% der Gesamtvarianz in der abhängigen Variablen wird durch Unterschiede zwischen den Teilnehmern erklärt.\n")

#############################

# Residuen des Nullmodells extrahieren
residuals_null <- residuals(null_model)

# Standardisierte Residuen berechnen
standardized_residuals <- residuals_null / sqrt(summary(null_model)$dispersion)

# Ausgabe der standardisierten Residuen
print(standardized_residuals)

# Residuen visualisieren
hist(standardized_residuals, breaks = 20, main = "Standardisierte Residuen", xlab = "Standardisierte Residuen")

# Streudiagramm der standardisierten Residuen gegen die Vorhersagen
predicted_values <- predict(null_model, type = "response")
plot(predicted_values, standardized_residuals, xlab = "Vorhergesagte Werte", ylab = "Standardisierte Residuen")
abline(h = 0, col = "red")

# Q-Q-Plot erstellen
qqnorm(standardized_residuals, main = "Q-Q-Plot der standardisierten Residuen")
qqline(standardized_residuals, col = "red")

# Shapiro-Wilk-Test durchführen
set.seed(123)  
sampled_residuals <- sample(standardized_residuals, size = min(5000, length(standardized_residuals)))
shapiro_test_result <- shapiro.test(sampled_residuals)
print(shapiro_test_result)

# Hinweis: Residuen folgen möglicherweise nicht der Normalverteilung

#############################

# Homoskedastizität (Residuenplot)
standardized_residuals_full <- residuals(full_model, type = "pearson")

# Residuenplot erstellen
plot(fitted(full_model), standardized_residuals_full, 
     xlab = "Fitted Values", ylab = "Standardized Residuals",
     main = "Residuals vs Fitted (Full Model)")
abline(h = 0, col = "red") 

#############################

# Lineare Beziehung auf der Logit-Skala (Box-Tidwell-Test)
boxTidwell(sustainable.choice ~ round.number + treatment.group + 
             num_geschlecht + num_studierende + num_einkauf + 
             num_politische_orientierung + num_einkommen,  
           data = df)

#############################

# Unabhängigkeit der Residuen (Durbin-Watson-Test)
# Berechne die Residuen des vollständigen Modells
residuals_full_model <- residuals(full_model, type = "pearson")

# Fitted values (vorhergesagte Werte) des vollständigen Modells
fitted_values_full_model <- fitted(full_model)

# Durbin-Watson-Test durchführen
dw_test <- dwtest(lm(residuals_full_model ~ fitted_values_full_model))
print(dw_test)

# DW = 2.1393: keine starke Autokorrelation in den Residuen
# p-value = 1
# Annahme der Unabhängigkeit der Residuen ist erfüllt

#############################

install.packages("ResourceSelection") 
library(ResourceSelection)

# Vorhersagen des Nullmodells erhalten
predicted_probs <- predict(null_model, type = "response")

# Hosmer-Lemeshow-Test durchführen
hoslem_test <- hoslem.test(df$sustainable.choice, predicted_probs, g = 10)  # g = Anzahl der Gruppen
print(hoslem_test)
# p-value < 2.2e-16: Modell passt nicht gut zu den Daten, deutet darauf hin, ass die Vorhersagen des Modells signifikant von den beobachteten Werten abweichen
