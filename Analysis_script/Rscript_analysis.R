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
library(blme)
library(gt)

# DATEN EINLESEN
load("/Users/ausleihe/Desktop/daten/Cleaned_data/df_wide_choice.Rdata")
load("/Users/ausleihe/Desktop/daten/Cleaned_data/df_long_memory.Rdata")
load("/Users/ausleihe/Desktop/daten/Cleaned_data/df_long_choice.Rdata")
load("/Users/ausleihe/Desktop/daten/Cleaned_data/df_tracking.Rdata")

# Setze das klassische Thema als Standard für alle Plots
theme_set(theme_classic())

#################################################################################################################################

# Anzahl Probanden 
count_probanden <- function(df_long_choice) {
  n_distinct(df_long_choice$participant.code)
}

# Probandenanzahl in jedem Datensatz ermitteln
anzahl_probanden_long_choice <- count_probanden(df_long_choice)
anzahl_probanden_wide_choice <- count_probanden(df_wide_choice)
anzahl_probanden_memory <- count_probanden(df_long_memory)
anzahl_probanden_tracking <- count_probanden(df_tracking)

# Ergebnisse ausgeben
cat("Anzahl der Probanden in df_long_choice:", anzahl_probanden_long_choice, "\n")
cat("Anzahl der Probanden in df_wide_choice:", anzahl_probanden_wide_choice, "\n")
cat("Anzahl der Probanden in df_long_memory:", anzahl_probanden_memory, "\n")
cat("Anzahl der Probanden in df_tracking:", anzahl_probanden_tracking, "\n")

#################################################################################################################################

# DESKRIPTIVE STATISTIK - Demografische Variablen

# Altersstatistik
summary_age <- summary(df_wide_choice$alter) 
print(summary_age)
sd_age <- sd(df_wide_choice$alter, na.rm = TRUE)

# Histogramm und Boxplot der Altersverteilung
ggplot(df_wide_choice, aes(x = alter)) +
  geom_histogram(binwidth = 1, fill = "grey", color = "black") +
  labs(title = "Altersverteilung", x = "Alter", y = "Häufigkeit")

ggplot(df_wide_choice, aes(y = alter)) +
  geom_boxplot(fill = "grey", color = "black") +
  labs(title = "Boxplot der Altersverteilung", y = "Alter")

# Häufigkeiten und Prozentsätze für Geschlecht und Ernährungsgewohnheiten
gender_summary <- table(df_wide_choice$geschlecht)
diet_summary <- df_wide_choice %>%
  group_by(ernährung) %>%
  summarise(count = n(), percent = (count / nrow(df_wide_choice)) * 100, .groups = 'drop')

# Häufigkeiten und Prozentsätze für politische Orientierung, Studierende, Haushaltseinkauf und Einkommen
politics_summary <- table(df_wide_choice$politische.O)
studis_summary <- table(df_wide_choice$studierende)
einkauf_summary <- table(df_wide_choice$einkauf)
income_summary <- table(df_wide_choice$einkommen)

# Kreuztabellen für Geschlecht und Ernährung, sowie Geschlecht und politische Orientierung
table_gender_diet <- table(df_wide_choice$geschlecht, df_wide_choice$ernährung)
print(table_gender_diet)
table_gender_politics <- table(df_wide_choice$geschlecht, df_wide_choice$politische.O)
print(table_gender_politics)

# Nachhaltige Entscheidungen pro politischer Gruppe und Geschlecht
sustainable_by_politicalGroup <- df_wide_choice %>%
  group_by(politische.O) %>%
  summarise(sum_sustainableChoice = sum(sum_sustainableChoice, na.rm = TRUE), count = n()) %>%
  mutate(percent_sustainable = (sum_sustainableChoice / (15 * count)) * 100)

sustainable_by_gender <- df_wide_choice %>%
  group_by(geschlecht) %>%
  summarise(sum_sustainableChoice = sum(sum_sustainableChoice, na.rm = TRUE), count = n()) %>%
  mutate(percent_sustainable = (sum_sustainableChoice / (15 * count)) * 100)

# Boxplot und Q-Q-Plot für Betrachtungsdauer im Tracking
boxplot(df_tracking$duration, main = "Boxplot der Dauer")
qqnorm(df_tracking$duration, main = "Q-Q-Plot der Dauer")
qqline(df_tracking$duration, col = "red")

#############################

# Wahl nachhaltiger vs. nicht-nachhaltiger Optionen insgesamt
choice_counts <- df_long_choice %>%
  group_by(sustainable.choice) %>%
  summarise(count = n(), .groups = 'drop')
print(choice_counts)

# Anzahl nachhaltiger und nicht nachhaltiger Entscheidungen pro Gruppe
choice_counts_by_group <- df_long_choice %>%
  group_by(treatment.group, sustainable.choice) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(choice_type = ifelse(sustainable.choice == 1, "Nachhaltig", "Nicht Nachhaltig"))
print(choice_counts_by_group)

# Visualisierung der Anzahl der Entscheidungen pro Gruppe
ggplot(choice_counts_by_group, aes(x = treatment.group, y = count, fill = choice_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Anzahl nachhaltiger und nicht nachhaltiger Entscheidungen", 
       x = "Gruppe", y = "Anzahl der Entscheidungen") +
  scale_fill_brewer(palette = "Set2")

#############################

# Prozentsätze nachhaltiger und nicht nachhaltiger Entscheidungen pro Gruppe
choice_percentages_by_group <- df_long_choice %>%
  group_by(treatment.group2) %>%
  summarise(
    count_sustainable = sum(sustainable.choice == 1),
    total_count = n(),
    nachhaltig = (count_sustainable / total_count) * 100,
    nicht_nachhaltig = 100 - nachhaltig,  
    .groups = 'drop'
  ) %>%
  select(treatment.group2, nachhaltig, nicht_nachhaltig)
print(choice_percentages_by_group)

#############################

# Mittelwerte und Standardabweichungen
sustainable_stats <- df_long_choice %>%
  group_by(treatment.group2) %>%
  summarise(
    mean_sustainable_choice = mean(sustainable.choice, na.rm = TRUE),
    sd_sustainable_choice = sd(sustainable.choice, na.rm = TRUE),
    .groups = 'drop'
  )
print(sustainable_stats)

# Häufigkeiten
treatment_freq <- table(df_long_choice$treatment.group2)
choice_freq <- table(df_long_choice$sustainable.choice)

print(treatment_freq)
print(choice_freq)

#################################################################################################################################

# GRAFISCHE DARSTELLUNG - Demopgrafische Variablen

# Balkendiagramm für Einkommen
income_freq <- df_wide_choice %>%
  count(einkommen) %>%
  mutate(percentage = n / sum(n) * 100)

ggplot(income_freq, aes(x = einkommen, y = n, fill = einkommen)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Häufigkeit des monatlichen Einkommens", x = "Monatliches Einkommen", y = "Häufigkeit") + 
  scale_fill_brewer(palette = "Set6") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 200))

#############################

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

# DESKRIPTIVE STATISTIK - Memory Task und Tracking 

# Häufigkeiten der nachhaltigen Entscheidungen und Attribute
sustainable_freq <- table(df_tracking$sustainable.choice)
attribute_freq <- table(df_tracking$attributeType)

#############################

# Durchschnittliche Abweichung im Memory Task
mean_absolut_diff_overall <- mean(df_long_memory$mean_absolut_diff, na.rm = TRUE)
print(mean_absolut_diff_overall)

# Durchschnittliche Abweichung pro Gruppe
mean_diff_group <- df_long_memory %>% 
  group_by(treatment.group) %>% 
  summarise(mean_diff = mean(mean_absolut_diff, na.rm = TRUE), 
            .groups = 'drop')
print(mean_diff_group)

# Visualisierung der durchschnittlichen Abweichung pro Gruppe
ggplot(mean_diff_group, aes(x = treatment.group, y = mean_diff, fill = treatment.group)) +
  geom_bar(stat = "identity") +
  labs(title = "Durchschnittliche Abweichung pro Gruppe", x = "Gruppe", y = "Durchschnittliche Abweichung") +
  scale_fill_brewer(palette = "Set2")

#############################

# Durchschnittliche Abweichung für bekannte vs. unbekannte CO2-Attribute
mean_diff_known <- mean(df_long_memory$absolut_diff[df_long_memory$round.number %in% 1:5], na.rm = TRUE)
mean_diff_unknown <- mean(df_long_memory$absolut_diff[df_long_memory$round.number %in% 6:7], na.rm = TRUE)

# Berechnung und Umstrukturierung für die Visualisierung
mean_diff_group_by_attributes <- df_long_memory %>%
  group_by(treatment.group) %>%
  summarise(
    mean_diff_known = mean(absolut_diff[round.number %in% 1:5], na.rm = TRUE),
    mean_diff_unknown = mean(absolut_diff[round.number %in% 6:7], na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_longer(
    cols = starts_with("mean_diff"),
    names_to = "category",
    values_to = "mean_difference"
  ) %>%
  mutate(
    category = dplyr::recode(category,
                             mean_diff_known = "Known Attributes",
                             mean_diff_unknown = "Unknown Attributes"
    )
  )

# Visualisierung der Abweichungen pro Gruppe
ggplot(mean_diff_group_by_attributes, aes(x = treatment.group, y = mean_difference, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Durchschnittliche Abweichung für bekannte vs. unbekannte CO2-Attribute", x = "Gruppe", y = "Durchschnittliche Abweichung") +
  scale_fill_brewer(palette = "Set2")

#############################

# Häufigstes und seltenstes gewähltes Lebensmittel
stimulus_counts <- df_long_choice %>%
  group_by(food.chosen) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(desc(count))

# Ergebnisse anzeigen
cat("Häufigster Stimulus:\n")
print(stimulus_counts %>% slice(1))
cat("\nSeltenster Stimulus:\n")
print(stimulus_counts %>% slice(n()))

#############################

# Durchschnittliche Betrachtungsdauer pro Attribut
duration_summary <- df_tracking %>%
  group_by(attributeType) %>%
  summarise(avg_duration = mean(duration, na.rm = TRUE),
            total_duration = sum(duration, na.rm = TRUE),
            count = n(), .groups = 'drop') %>%
  arrange(desc(avg_duration))

# Visualisierung der durchschnittlichen Betrachtungsdauer
ggplot(duration_summary, aes(x = reorder(attributeType, -avg_duration), y = avg_duration)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Durchschnittliche Betrachtungsdauer pro Attribut", x = "Attributtyp", y = "Durchschnittliche Dauer (Millisekunden)")

#############################

# Durchschnittliche und gesamte Betrachtungsdauer pro Attribut und Gruppe
duration_summary_group <- df_tracking %>%
  group_by(attributeType, treatment.group) %>%
  summarise(avg_duration = mean(duration, na.rm = TRUE),
            total_duration = sum(duration, na.rm = TRUE),
            count = n(), .groups = 'drop')

# Visualisierung der durchschnittlichen Betrachtungsdauer nach Attribut und Gruppe
ggplot(duration_summary_group, aes(x = attributeType, y = avg_duration, fill = treatment.group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Durchschnittliche Betrachtungsdauer pro Attribut und Gruppe", x = "Attributtyp", y = "Durchschnittliche Dauer (Millisekunden)") +
scale_fill_brewer(palette = "Set2")

#############################

# Klicks für jede attributeType pro Gruppe
clicks_summary <- df_tracking %>%
  group_by(attributeType, treatment.group) %>%
  summarise(click_count = n(), .groups = 'drop')

# Visualisierung der Klicks pro Attribut und Gruppe
ggplot(clicks_summary, aes(x = attributeType, y = click_count, fill = treatment.group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Anzahl der Klicks auf Attribute pro Gruppe", x = "Attribut", y = "Anzahl der Klicks") +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

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
  mutate(across(c(sustainability_score, CO2.diff), ~ scale(.)[ ,1]))

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
mod4 <- glmer(sustainable.choice ~ treatment.group * sustainability_score + 
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
emm <- emmeans(mod4, ~ treatment.group | sustainability_score)
contrast(emm, method = "pairwise", by = "sustainability_score", adjust = "bonferroni")

# Visualisierung der Kontraste
# Wahrscheinlichkeit, dass Teilnehmer eine nachhaltige Wahl treffen, in Abhängigkeit von der Behandlungsgruppe (Kontrollgruppe vs. Experimentalgruppe)
emm_df <- as.data.frame(emm)
emm_df$probability <- plogis(emm_df$emmean) 

# Konfidenzintervalle berechnen
emm_df$lower_ci <- plogis(emm_df$emmean - (1.96 * emm_df$SE))  # Unteres CI
emm_df$upper_ci <- plogis(emm_df$emmean + (1.96 * emm_df$SE))  # Oberes CI

# Plot erstellen
ggplot(emm_df, aes(x = treatment.group, y = probability, color = treatment.group)) +
  geom_point(size = 3) +  # Punkte anzeigen
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +  # Konfidenzintervalle
  labs(title = "Wahrscheinlichkeit der nachhaltigen Wahl nach Behandlungsgruppe",
       x = "Behandlungsgruppe",
       y = "Wahrscheinlichkeit der nachhaltigen Wahl")

#################################################################################################################################

# H4: MULTILEVEL REGRESSION ANALYSIS: RECALL ACCURACY

# Hinzufügen der Variable, ob die Kohlenstoffattribute, die beim Memory-Test abgefragt werden, angesehen wurden
interested_stimuli <- c(7, 8, 14, 9, 11)

df_trackingCO2 <- df_tracking %>%
  group_by(participant.id) %>%
  summarise(
    carbon_viewed = ifelse(any(attributeType == "co2e/kg" & stimulusID %in% interested_stimuli), 1, 0),
    .groups = 'drop'
  )

df_trackingCO2$participant.id <- as.character(df_trackingCO2$participant.id)

df_long_choice <- df_long_choice %>%
  left_join(df_trackingCO2, by = "participant.id")

#############################

model5 <- lmer(mean_absolut_diff ~ treatment.group + carbon_viewed + sustainability_score + (1 | participant.id), 
               data = df_long_choice,
               control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)))
summary(model5)

# Visualisierung: Scatterplot für sustainability_score
ggplot(df_long_choice, aes(x = sustainability_score, y = mean_absolut_diff, color = treatment.group)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +  
  labs(title = "Mean Absolute Difference vs. Sustainability Score",
       x = "Sustainability Score",
       y = "Mean Absolute Difference")

# Visualisierung: Scatterplot für carbon_viewed
ggplot(df_long_choice, aes(x = carbon_viewed, y = mean_absolut_diff, color = treatment.group)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Mean Absolute Difference vs. Carbon Viewed",
       x = "Carbon Viewed",
       y = "Mean Absolute Difference")

#############################

# Multilevel-Regression für Recall Accuracy abhängig davon, ob das Carbon Attribut gesehen wurde sowie von den Umwelt-Werten
# NAs in carbon_viewed entfernen? Probanden in df_long_choice enthalten aber nicht in df_tracking 
df_long_choice <- df_long_choice %>%
  mutate(carbon_viewed = replace_na(carbon_viewed, 0))

model_recall <- lmer(mean_absolut_diff ~ treatment.group + carbon_viewed + sustainability_score + 
                             (1 | participant.id), 
                             data = df_long_choice, 
                             control = lmerControl(optimizer = "nloptwrap"))
summary(model_recall)

# Interaktioneffekte: Gruppenzugehörigkeit und Ansehen des Carbon Attributs auf Recall Accuracy 
mod_interaction_recall <- lmer(mean_absolut_diff ~ treatment.group * carbon_viewed + 
                                 (1 | participant.id) + (1 | round.stimuliID), 
                               data = df_long_choice)
summary(mod_interaction_recall)

# Kontraste zur Untersuchung der Gruppenunterschiede
emm <- emmeans(mod_interaction_recall, ~ treatment.group | sustainability_score)
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
control <- df_long_choice %>% filter(treatment.group == "control")
label <- df_long_choice %>% filter(treatment.group == "label")
norm <- df_long_choice %>% filter(treatment.group == "norm")

#############################

# T-Tests zwischen den Gruppen
controllabel <- t.test(control$sustainable.choice, label$sustainable.choice) 
print(controllabel) 
# Unterschied zwischen den Mittelwerten der nachhaltigen Entscheidungen in der Kontroll- und Labelgruppe statistisch nicht signifikant

controlnorm <- t.test(control$sustainable.choice, norm$sustainable.choice) 
print(controlnorm) 
# Unterschied zwischen den Mittelwerten der nachhaltigen Entscheidungen in der Kontroll- und Normgruppe statistisch nicht signifikant

# ANOVA für nachhaltige Entscheidungen zwischen den Gruppen
anova_model <- aov(sustainable.choice ~ treatment.group, data = df_long_choice) 
print(summary(anova_model)) 
# Kein signifikanter Unterschied 

# ANOVA für Recall Accuracy
anova_recall_accuracy <- aov(mean_absolut_diff ~ treatment.group2, data = df_long_choice) 
print(summary(anova_recall_accuracy)) 
# Äusserst signifikanter Unterschied in der Recall Accuracy zwischen den Behandlungsgruppen

# ANOVA für Pro-Environmental Attitudes
anova_pro_env <- aov(sustainability_score ~ treatment.group, data = df_long_choice) 
print(summary(anova_pro_env)) 
# Sehr signifikanter Unterschied zwischen den Mittelwerten der Nachhaltigkeitswerte in den verschiedenen Behandlungsgruppen

#################################################################################################################################

# SEKUNDÄRE ANALYSEN 

# 1. Recall Performance
# Analyse der Recall-Performance in Abhängigkeit von den Umwelteinstellungen
# df_long_choice$mean_absolut_diff <- scale(df_long_choice$mean_absolut_diff)
# df_long_choice$sustainability_score <- scale(df_long_choice$sustainability_score)
# df_long_choice$participant.id <- as.factor(df_long_choice$participant.id)

model_recall2 <- lmer(mean_absolut_diff ~ sustainability_score + treatment.group + 
                        (1 | participant.id), 
                      data = df_long_choice, 
                      control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)))
summary(model_recall2)

# 2. Impact of Pro-environmental Attitudes
# Vergleich der Recall-Leistung basierend auf den Umweltwerten
model_pro_env <- lmer(mean_absolut_diff ~ sustainability_score + (1 | participant.id), data = df_long_choice)
summary(model_pro_env)

# Visualisierung der Recall-Leistung in Abhängigkeit von den Umweltwerten
ggplot(df_long_choice, aes(x = sustainability_score, y = mean_absolut_diff)) +
  geom_point(aes(color = treatment.group)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Recall Performance vs. Pro-Environmental Attitudes", 
       x = "Pro-Environmental Attitudes", 
       y = "Recall Performance (Absolute Difference)")

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
       y = "Durchschnittlicher Carbon View")

#############################

# 4. Predictive Relationships
# Analyse, ob Umweltbedenken das Suchverhalten vorhersagen können
model_search_behavior <- glmer(
  sustainable.choice ~ sustainability_score + carbon_viewed + (1 | participant.id),
  data = df_long_choice,
  family = binomial,
  control = glmerControl(optCtrl = list(maxfun = 1e5), check.conv.grad = .makeCC("warning", tol = 0.005))
)
summary(model_search_behavior)
# positive und signifikante Beziehung zwischen dem sustainability_score und der Wahrscheinlichkeit, eine nachhaltige Wahl zu treffen, zeigt, dass Teilnehmer mit höheren Bewertungen in Bezug auf Nachhaltigkeit eher nachhaltige Entscheidungen treffen
# Schätzwert für carbon_viewed ist positiv, aber nicht signifikant

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

# Umwandeln in einen Faktor
df_long_choice$participant.id <- as.factor(df_long_choice$participant.id)
first_viewed$participant.id <- as.factor(first_viewed$participant.id)
last_viewed$participant.id <- as.factor(last_viewed$participant.id)

# Mergen der neuen Variablen mit dem Hauptdatensatz
df_long_choice <- df_long_choice %>%
  left_join(first_viewed, by = c("participant.id", "round.number")) %>%
  left_join(last_viewed, by = c("participant.id", "round.number"))

#############################

# Explorative Analyse: Suchen von Mustern in der Informationssuche und deren Einfluss auf Essensentscheidungen
model_choice_patterns <- glmer(sustainable.choice ~ first_attribute_viewed + last_attribute_viewed + (1 | participant.id), 
                               data = df_long_choice, 
                               family = binomial, 
                               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(model_choice_patterns)

# Einfluss dessen, ob in der vorherigen Rund das CO2 Attribut zuletzt gesehen wurde darauf, ob in der nächsten Runde vermehrt die Nachhaltige Wahl getroffen wurde
df_long_choice <- df_long_choice %>%
  mutate(previous_CO2_viewed = lag(ifelse(last_attribute_viewed == "B_co2e/kg", 1, 0), order_by = round.number))

model_CO2_effect <- glmer(sustainable.choice ~ previous_CO2_viewed + round.number + 
                            (1 | participant.id), 
                          data = df_long_choice, 
                          family = binomial, 
                          control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(model_CO2_effect)
# previous_CO2_viewed: Positiver Schätzwert aber nicht signfikant
# Leichter Anstieg in der Wahrscheinlichkeit, eine nachhaltige Wahl zu treffen, wenn das CO2-Attribut in der vorherigen Runde zuletzt angesehen wurde

# 5. Robustness Check
# Exklusion der Teilnehmer, die den Verständnischeck beim ersten Versuch bestanden haben
df_excluded <- df_long_choice %>% filter(comp.check1== 0)  

# Wiederholung der Analysen nach Ausschluss
model_recall_excluded <- lmer(mean_absolut_diff ~ sustainability_score + treatment.group + (1 | participant.id),
                              data = df_excluded,
                              control = lmerControl(optimize = "bobyqa"))
summary(model_recall_excluded)

model_pro_env_excluded <- lmer(mean_absolut_diff ~ sustainability_score + (1 | participant.id), data = df_excluded)
summary(model_pro_env_excluded)

model_search_behavior_excluded <- glmer(sustainable.choice ~ sustainability_score + carbon_viewed + (1 | participant.id), 
                                        data = df_excluded, 
                                        family = binomial)
summary(model_search_behavior_excluded)

model_choice_patterns_excluded <- glmer(sustainable.choice ~ first_attribute_viewed + last_attribute_viewed + (1 | participant.id), 
                                        data = df_excluded, 
                                        family = binomial)
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
