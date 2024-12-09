library(readr)
library(dplyr)
library(ggplot2)
library(psych)
library(lme4)
library(lmerTest)
library(emmeans)
library(corrplot)
library(car)
library(tidyr)
library(stringr)
library(tibble)
library(blme)
library(gt)
library(Hmisc)
library(lmerTest)
library(sjPlot)
library(DescTools)
library(ggimage)
library(ggrepel)

# DATEN EINLESEN
load("/Users/ausleihe/data_analysis_master_lara/Cleaned_data/df_wide_choice.Rdata")
load("/Users/ausleihe/data_analysis_master_lara/Cleaned_data/df_long_memory.Rdata")
load("/Users/ausleihe/data_analysis_master_lara/Cleaned_data/df_long_choice.Rdata")
load("/Users/ausleihe/data_analysis_master_lara/Cleaned_data/df_tracking.Rdata")
load("/Users/ausleihe/data_analysis_master_lara/Cleaned_data/df_long_memory2.Rdata")

# Funktion zur Zählung der einzigartigen Probanden
count_probanden <- function(data, id_column) {
  if (id_column %in% colnames(data)) {
    return(length(unique(data[[id_column]])))
  } else {
    return(NA)
  }
}

# Zählung der Probanden für jeden Datensatz
cat("Anzahl der Probanden in df_wide_choice:", count_probanden(df_wide_choice, "participant.id"), "\n")
cat("Anzahl der Probanden in df_long_memory:", count_probanden(df_long_memory, "participant.id"), "\n")
cat("Anzahl der Probanden in df_long_choice:", count_probanden(df_long_choice, "participant.id"), "\n")
cat("Anzahl der Probanden in df_tracking:", count_probanden(df_tracking, "participant.id"), "\n")
cat("Anzahl der Probanden in df_long_memory2:", count_probanden(df_long_memory2, "participant.id"), "\n")

#################################################################################################################################

# Setze das klassische Thema als Standard für alle Plots
theme_set(theme_classic())

all_ids <- list(
  df_tracking$participant.id,
  df_long_choice$participant.id,
  df_wide_choice$participant.id,
  df_long_memory$participant.id,
  df_long_memory2$participant.id
)
final_participants <- Reduce(intersect, all_ids)
cat("Anzahl der finalen gemeinsamen Probanden:", length(final_participants), "\n")
# 528

#################################################################################################################################

# DESKRIPTIVE STATISTIK - Demografische Variablen

# Anzahl der Probanden pro Gruppe
probanden_anzahl <- df_long_choice_filtered %>%
  group_by(treatment.group) %>%
  summarise(anzahl_probanden = n_distinct(participant.id))
print(probanden_anzahl)

# Altersstatistik
summary_age <- summary(df_wide_choice_filtered$alter) 
print(summary_age)
sd_age <- sd(df_wide_choice_filtered$alter, na.rm = TRUE)

# Häufigkeiten und Prozentsätze für Geschlecht und Ernährungsgewohnheiten
gender_summary <- table(df_wide_choice_filtered$geschlecht)
diet_summary <- df_wide_choice_filtered %>%
  group_by(ernährung) %>%
  summarise(count = n(), percent = (count / nrow(df_wide_choice_filtered)) * 100, .groups = 'drop')
print(diet_summary)

# Häufigkeiten und Prozentsätze für politische Orientierung, Studierende, Haushaltseinkauf und Einkommen
politics_summary <- table(df_wide_choice_filtered$politische.O)
studis_summary <- table(df_wide_choice_filtered$studierende)
einkauf_summary <- table(df_wide_choice_filtered$einkauf)
income_summary <- table(df_wide_choice_filtered$einkommen)

# Kreuztabellen für Geschlecht und Ernährung, sowie Geschlecht und politische Orientierung
table_gender_diet <- table(df_wide_choice_filtered$geschlecht, df_wide_choice_filtered$ernährung)
print(table_gender_diet)
table_gender_politics <- table(df_wide_choice_filtered$geschlecht, df_wide_choice_filtered$politische.O)
print(table_gender_politics)

# Geschlecht x pol.Orientierung
total_female <- sum(table_gender_politics["Weiblich", ])
female_percentages <- table_gender_politics["Weiblich", ] / total_female * 100
print(female_percentages)

total_male <- sum(table_gender_politics["Männlich", ])
male_percentages <- table_gender_politics["Männlich", ] / total_male * 100
print(male_percentages)

# Nachhaltige Entscheidungen pro politischer Gruppe und Geschlecht
sustainable_by_politicalGroup <- df_wide_choice_filtered %>%
  group_by(politische.O) %>%
  summarise(sum_sustainableChoice = sum(sum_sustainableChoice, na.rm = TRUE), count = n()) %>%
  mutate(percent_sustainable = (sum_sustainableChoice / (15 * count)) * 100)
print(sustainable_by_politicalGroup)

sustainable_by_gender <- df_wide_choice_filtered %>%
  group_by(geschlecht) %>%
  summarise(sum_sustainableChoice = sum(sum_sustainableChoice, na.rm = TRUE), count = n()) %>%
  mutate(percent_sustainable = (sum_sustainableChoice / (15 * count)) * 100)
print(sustainable_by_gender)

# Grouped Bar Chart for Sustainable Choices by Gender
ggplot(sustainable_by_gender, aes(x = geschlecht, y = percent_sustainable, fill = geschlecht)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percent_sustainable, 1), "%")), 
            vjust = -0.5, size = 3.5) + # Add labels above bars
  labs(title = "Sustainable Choices by Gender", x = "Gender", y = "Percentage of Sustainable Choices") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
  scale_fill_brewer(palette = "Set2")

#############################

# Sustainability Score einzeln 
df_wide_choice_filtered <- df_wide_choice_filtered %>%
  mutate(
    climate_concern = rowMeans(across(c("frage_1", "frage_2", "frage_3", "frage_4")), na.rm = TRUE),
    pro_env_norms = rowMeans(across(c("frage_5", "frage_6")), na.rm = TRUE),
    social_norms = rowMeans(across(c("frage_7", "frage_8")), na.rm = TRUE)
  )

# Zusammenfassung der Subskalen
print(list(
  ClimateConcern = summary(df_wide_choice_filtered$climate_concern),
  ProEnvNorms = summary(df_wide_choice_filtered$pro_env_norms),
  SocialNorms = summary(df_wide_choice_filtered$social_norms)
))

# Cronbach's Alpha für alle Items und jede Subskala
all_items <- df_wide_choice_filtered[, c("frage_1", "frage_2", "frage_3", "frage_4", 
                                         "frage_5", "frage_6", "frage_7", "frage_8")]
print(psych::alpha(all_items))

print(list(
  Gesamt = psych::alpha(all_items),
  ClimateConcern = psych::alpha(df_wide_choice_filtered[, c("frage_1", "frage_2", "frage_3", "frage_4")]),
  ProEnvNorms = psych::alpha(df_wide_choice_filtered[, c("frage_5", "frage_6")]),
  SocialNorms = psych::alpha(df_wide_choice_filtered[, c("frage_7", "frage_8")])
))

# Deskriptive Statistiken der Fragebögen
fragebogen_stats <- lapply(
  c("score_fragebogen_1", "score_fragebogen_2", "score_fragebogen_3"),
  function(var) describe(df_wide_choice_filtered[[var]])
)
names(fragebogen_stats) <- c("Fragebogen1", "Fragebogen2", "Fragebogen3")
print(fragebogen_stats)

# Cronbach’s Alph von den ersten beiden Fragebögen: 0.88
# Climate Concern: 0.88
# Pro-Environmental Personal Norms: 0.87
# Social Norms: 0.75




#################################################################################################################################

# GRAFISCHE DARSTELLUNG - Demografische Variablen

# Histogramm und Boxplot der Altersverteilung
ggplot(df_wide_choice_filtered, aes(x = alter)) +
  geom_histogram(binwidth = 1, fill = "grey", color = "black") +
  labs(title = "Altersverteilung", x = "Alter", y = "Häufigkeit")
ggplot(df_wide_choice_filtered, aes(y = alter)) +
  geom_boxplot(fill = "grey", color = "black") +
  labs(title = "Boxplot der Altersverteilung", y = "Alter")

# Balkendiagramm für Einkommen
income_freq <- df_wide_choice_filtered %>%
  count(einkommen) %>%
  mutate(percentage = n / sum(n) * 100)

ggplot(income_freq, aes(x = einkommen, y = n, fill = einkommen)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Häufigkeit des monatlichen Einkommens", x = "Monatliches Einkommen", y = "Häufigkeit") + 
  scale_fill_brewer(palette = "Set6") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 200))

# Boxplot für Einkommen nach Geschlecht
ggplot(df_wide_choice_filtered, aes(x = factor(num_geschlecht), y = num_einkommen)) +
  geom_boxplot() +
  labs(title = "Einkommen nach Geschlecht", x = "Geschlecht", y = "Einkommen")

# Korrelation abhängig von der Ernährung
model <- lm(num_ernährung ~ num_geschlecht + num_studierende + num_einkauf + num_politische_orientierung + num_einkommen, data = df_wide_choice_filtered)
summary(model)
# Insbesondere zeigen Frauen und Menschen mit linker politischer Orientierung umweltfreundlichere Ernährungsgewohnheiten im Vergleich zu Männern und Menschen mit rechter politischer Orientierung. 
# Ein höheres Einkommen ist ebenfalls mit weniger umweltfreundlichen Ernährungsgewohnheiten assoziiert 
# Hier habe ich diet_preference entfernt, da ich ja nur mit Vegan und vegetarisch arbeite, aber eigentlich muss das hier schon noch rein

# Korrelationsmatrix für alle Daten (numerisch)
cor_matrix <- cor(df_wide_choice_filtered %>% select(num_einkommen, num_geschlecht, num_studierende, num_einkauf, num_politische_orientierung, sustainability_score), use = "complete.obs")
print(cor_matrix)
summary(cor_matrix)
colnames(cor_matrix) <- c("Income", "Gender", "Students", "Shopping", "Political Orientation", "Sustainability Score")
rownames(cor_matrix) <- colnames(cor_matrix)
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.cex = 0.7, diag = FALSE, addCoef.col = "black")

# Korr. Matrix nur mit signifikanten Werten
result <- rcorr(as.matrix(df_wide_choice_filtered %>% 
                            select(num_einkommen, num_geschlecht, num_studierende, num_einkauf, num_politische_orientierung, sustainability_score)))
# num_ernährung, 
corrmatrix <- result$r
p_matrix <- result$P
colnames(cor_matrix) <- c("Income", "Gender", "Students", "Shopping", "Political Orientation", "Sustainability Score")
# "Diet Preference" 
rownames(cor_matrix) <- colnames(cor_matrix)
colnames(p_matrix) <- colnames(cor_matrix)
rownames(p_matrix) <- rownames(cor_matrix)
# Zeigt nur signifikante Korrelationen (p < 0,05)
corrplot(cor_matrix, method = "circle", type = "upper", 
         p.mat = p_matrix, sig.level = 0.05, insig = "blank",
         tl.col = "black", tl.cex = 0.7, diag = FALSE, addCoef.col = "black")




#################################################################################################################################

# DESKRIPTIVE STATISTIK - Choice Task

# Häufigkeiten über alle Probanden und alle Runden hinweg 
treatment_freq <- table(df_long_choice_filtered$treatment.group2)
print(treatment_freq)

choice_freq <- table(df_long_choice_filtered$sustainable.choice)
print(choice_freq)

# Anzahl nachhaltiger und nicht nachhaltiger Entscheidungen pro Gruppe
choice_counts_by_group <- df_long_choice_filtered %>%
  group_by(treatment.group2, sustainable.choice) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(choice_type = ifelse(sustainable.choice == 1, "Nachhaltig", "Nicht Nachhaltig"))
print(choice_counts_by_group)
# oder treatment.group, um drei Gruppen zu erhalten

#############################

# Wahrscheinlichkeit der Lebensmittelwahl
df_long_choice_filtered %>% select(NameA, NameB, PriceA, PriceB, price.diff, ProteinA, ProteinB,protein.diff, CO2.diff, CO2A, CO2B, round.stimuliID, sustainable.left, sustainable.choice) %>% group_by(NameA, NameB, round.stimuliID, sustainable.left) %>% summarise(meanChoice = mean(sustainable.choice))
# Percentage of sustainable food chosen
df_long_choice_filtered %>% select(NameA, NameB, PriceA, PriceB, price.diff, ProteinA, ProteinB,protein.diff, CO2.diff, CO2A, CO2B, round.stimuliID, sustainable.left, sustainable.choice) %>% group_by(NameA, NameB, round.stimuliID) %>% summarise(meanChoice = mean(sustainable.choice))
## Make a by food dictonary
fooddf <- df_long_choice_filtered %>% select(NameA, NameB, PriceA, PriceB, price.diff, ProteinA, ProteinB,protein.diff, CO2.diff, CO2A, CO2B, round.stimuliID, sustainable.left) %>% unique()

#############################

# Vergleich der nachhaltigen Entscheidungen zwischen Kontroll- und Experimentalgruppe
control <- df_long_choice_filtered %>% filter(treatment.group == "control")
experimental <- df_long_choice_filtered %>% filter(treatment.group != "control")

# T-Test zwischen der Kontroll- und Experimentalgruppe
t_test_result <- t.test(control$sustainable.choice, experimental$sustainable.choice)
print(t_test_result)  # p-Wert von 0.08948: marginaler nicht signifikanter Unterschied zwischen den nachhaltigen Entscheidungen der Kontroll- und Experimentalgruppe

# ANOVA für Recall Accuracy
anova_recall_accuracy <- aov(relative_diff ~ treatment.group, data = df_long_memory_filtered2)
print(summary(anova_recall_accuracy))  # extrem signifikanten Unterschied zwischen den Behandlungsgruppen: unterschiedlichen Behandlungsgruppen haben einen signifikanten Einfluss auf die Recall Accuracy

tukey_result <- TukeyHSD(anova_recall_accuracy)
print(tukey_result)

# ANOVA für Pro-Environmental Attitudes
anova_pro_env <- aov(sustainability_score ~ treatment.group2, data = df_long_choice_filtered)
print(summary(anova_pro_env))  # Signifikanten Unterschied in den Pro-Environmental Attitudes zwischen den Behandlungsgruppen (p = 0.00904)

#############################

# Berechne den Anteil nachhaltiger Entscheidungen pro Gruppe
df_summary <- df_long_choice_filtered %>%
  group_by(treatment.group) %>%
  summarise(
    mean_sustainable_choice = mean(sustainable.choice, na.rm = TRUE) * 100
  )
print(df_summary)

# Wahrscheinlichkeiten und Konfidenzintervalle berechnen
probabilities <- df_long_choice_filtered %>%
  group_by(round.number, treatment.group) %>%
  summarise(
    prob_sustainable_choice = mean(sustainable.choice, na.rm = TRUE),
    se = sd(sustainable.choice, na.rm = TRUE) / sqrt(n()), 
    .groups = 'drop'
  ) %>%
  mutate(
    lower_ci = prob_sustainable_choice - 1.96 * se, # Unteres Konfidenzintervall
    upper_ci = prob_sustainable_choice + 1.96 * se  # Oberes Konfidenzintervall
  )

# Konfidenzintervalle auf Wahrscheinlichkeitsbereich beschränken (0-1)
probabilities <- probabilities %>%
  mutate(
    lower_ci = pmax(lower_ci, 0), # Werte unter 0 auf 0 setzen
    upper_ci = pmin(upper_ci, 1)  # Werte über 1 auf 1 setzen
  )

# Grafik erstellen
ggplot(probabilities, aes(x = round.number, y = prob_sustainable_choice, color = treatment.group)) +
  geom_line() + 
  geom_point() +  
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = treatment.group), alpha = 0.2, color = NA) + # Konfidenzintervalle
  labs(
    title = "Probability of Sustainable Choice by Treatment Group",
    x = "Round",
    y = "Probability of Sustainable Choice"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  scale_fill_manual(values = c("grey", "blue", "green")) 


#############################

# Wahrscheinlichkeiten für nachhaltige Wahl pro Gruppe und in Abh. von sustainability_score
correlations <- df_long_choice_filtered %>%
  group_by(treatment.group2) %>%
  summarise(
    pearson_corr = cor(sustainability_score, sustainable.choice, method = "pearson", use = "complete.obs"),
    n = n(),
    .groups = "drop"
  )

# Ausgabe der berechneten Korrelationen
cat("Berechnete Korrelationen pro Gruppe:\n")
print(correlations)

# Extrahiere Korrelationswerte und Gruppengrößen
corr_exp <- correlations$pearson_corr[correlations$treatment.group2 == "experimental"]
corr_ctrl <- correlations$pearson_corr[correlations$treatment.group2 == "control"]
n_exp <- correlations$n[correlations$treatment.group2 == "experimental"]
n_ctrl <- correlations$n[correlations$treatment.group2 == "control"]

# Fisher-Z-Transformation der Korrelationen
z_exp <- atanh(corr_exp)  # Fisher-Z für experimentelle Gruppe
z_ctrl <- atanh(corr_ctrl)  # Fisher-Z für Kontrollgruppe

# Standardfehler der Z-Transformationen
se_diff <- sqrt(1 / (n_exp - 3) + 1 / (n_ctrl - 3))

# Z-Teststatistik
z_stat <- (z_exp - z_ctrl) / se_diff

# P-Wert für zweiseitigen Test
p_value <- 2 * (1 - pnorm(abs(z_stat)))

# Ergebnisse ausgeben
cat("Z-Statistik:", z_stat, "\n")
cat("P-Wert:", p_value, "\n")

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

# Mean clicks for each attributeType per person and group
clicks_summary <- df_tracking %>%
  group_by(attributeType, treatment.group2, participant.id) %>%  # Include participant_id for mean calculation
  summarise(click_count = n(), .groups = 'drop') %>%
  group_by(attributeType, treatment.group2) %>%  # Group again to calculate mean
  summarise(mean_click_count = mean(click_count), .groups = 'drop')  # Calculate mean

# Visualization of mean clicks per attribute and group
ggplot(clicks_summary, aes(x = attributeType, y = mean_click_count, fill = treatment.group2)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Clicks on Attributes by Group", x = "Attribute", y = "Mean Clicks") +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#############################

# Probanden, die auf kein Attribut geklickt haben
no_clicks <- df_tracking %>%
  group_by(participant.id) %>%  
  summarise(total_clicks = n(), .groups = 'drop') %>%  
  filter(total_clicks == 0) 

# Anzahl der Probanden, die auf kein Attribut geklickt haben
num_no_clicks <- nrow(no_clicks)
print(num_no_clicks)
# 0

#############################

# Weitere Plots zur Visualisierung der Daten
ggcol3 <- scale_color_manual(values = c("red", "blue", "green"))  

ggplot(df_long_choice_filtered, aes(round.number, sustainable.choice, na.rm = TRUE)) + 
  stat_smooth(aes(group = participant.id, color = treatment.group), method = "lm", formula = y ~ x, se = FALSE, na.rm = TRUE, size = 0.1) + 
  stat_smooth(aes(group = treatment.group, color = treatment.group), method = "lm", formula = y ~ x, se = FALSE, na.rm = TRUE, size = 1.5, linetype = "solid") + 
  ggcol3 + ylab("choice") + xlab("rounds")

ggplot(df_long_choice_filtered, aes(round.number, sustainable.choice, na.rm = TRUE)) + 
  stat_smooth(aes(group = participant.id, color = treatment.group), method = "loess", formula = y ~ x, se = FALSE, span = 1, na.rm = TRUE, size = 0.1) + 
  stat_smooth(aes(group = treatment.group, color = treatment.group), method = "loess", formula = y ~ x, span = 1, se = FALSE, na.rm = TRUE, size = 1.5, linetype = "solid") + 
  ggcol3 + facet_wrap(~treatment.group) + ylab("choice") + xlab("rounds") 

ggplot(data = df_long_choice_filtered[as.numeric(substr(df_long_choice_filtered$participant.id, 1, 5)) %% 10 == 0,], aes(x = round.number, y = sustainable.choice, group = participant.id)) + 
  stat_summary(aes(color = treatment.group), fun = mean, geom = "point", size = 0.5) + 
  stat_smooth(aes(color = treatment.group), method = "loess", formula = y ~ x, span = 1, se = FALSE, na.rm = TRUE, size = 0.3, linetype = "dashed") + 
  stat_smooth(aes(color = treatment.group), method = "lm", formula = y ~ x, se = FALSE, na.rm = TRUE, size = 0.3) + 
  facet_wrap(~participant.id) + ggcol3 + ylab("choice") + xlab("rounds") + ylim(0, 1)




#################################################################################################################################

# DESKRIPTIVE STATISTIK - Memory Task

# Durchschnittliche Abweichung im Memory Task
mean_absolut_diff_overall <- mean(df_long_memory_filtered$mean_absolut_diff, na.rm = TRUE)
print(mean_absolut_diff_overall)

# Durchschnittliche Abweichung pro Gruppe
mean_diff_group <- df_long_memory_filtered %>% 
  group_by(treatment.group) %>% 
  summarise(mean_diff = mean(mean_absolut_diff, na.rm = TRUE), 
            .groups = 'drop')
print(mean_diff_group)

# Visualization of mean deviation by group
ggplot(mean_diff_group, aes(x = treatment.group, y = mean_diff, fill = treatment.group)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Deviation in grams by Group", x = "Group", y = "Mean Deviation in grams") +
  scale_fill_brewer(palette = "Set2")

#############################

# Durchschnittliche Über- und Unterschätzung pro Option (gesamt)
average_over_under_all <- df_long_memory_filtered %>%
  summarise(
    mean_over_under_sustainable = mean(diff_Sustainable, na.rm = TRUE),
    mean_over_under_nonSustainable = mean(diff_nonSustainable, na.rm = TRUE)
  )
print(average_over_under_all)

# Berechnung des Prozentsatzes der korrekten Identifizierungen
percent_correct_identification <- df_long_memory_filtered2 %>%
  summarise(correct_percentage = mean(correct_identification) * 100)
print(percent_correct_identification)

df_long_memory_filtered <- df_long_memory_filtered %>%
  mutate(round.number = as.numeric(round.number))

# Vergleich der Leistung in den ersten 5 vs. letzten 2 Runden
# Daten für die ersten 5 und letzten 2 Runden filtern
first_5_rounds <- df_long_memory_filtered %>% filter(round.number <= 5)
last_2_rounds <- df_long_memory_filtered %>% filter(round.number > (max(round.number) - 2))

# Durchschnittliche real_diff berechnen
average_real_diff <- tibble(
  group = c("First 5 Rounds", "Last 2 Rounds"),
  average_real_diff = c(
    mean(first_5_rounds$real_diff, na.rm = TRUE) / 5,
    mean(last_2_rounds$real_diff, na.rm = TRUE) / 2
  )
)
print(average_real_diff)

# Durchschnittliche Über- und Unterschätzung pro Option für die ersten 5 und letzten 2 Runden
average_over_under_rounds <- bind_rows(
  first_5_rounds %>% summarise(
    group = "First 5 Rounds",
    mean_over_under_sustainable = mean(diff_Sustainable, na.rm = TRUE),
    mean_over_under_nonSustainable = mean(diff_nonSustainable, na.rm = TRUE)
  ),
  last_2_rounds %>% summarise(
    group = "Last 2 Rounds",
    mean_over_under_sustainable = mean(diff_Sustainable, na.rm = TRUE),
    mean_over_under_nonSustainable = mean(diff_nonSustainable, na.rm = TRUE)
  )
)
print(average_over_under_rounds)

#############################

# Vergleich zwischen geschätzten und tatsächlichen Werten
# Kombination von Lebensmittelname und korrektem CO2e-Wert für die x-Achse
df_long_memory_filtered2 <- df_long_memory_filtered2 %>%
  mutate(
    name_co2e = paste0(name, " (CO2e: ", round(correct, 0), ")")
  )

# Sortieren der Namen nach Runden
df_long_memory_filtered2$name_co2e <- factor(df_long_memory_filtered2$name_co2e, 
                                             levels = unique(df_long_memory_filtered2$name_co2e[order(df_long_memory_filtered2$round.number)]))

df_long_memory_filtered2 <- df_long_memory_filtered2 %>%
  mutate(prev_seen = factor(prev_seen, levels = c(0, 1), labels = c("Not Seen", "Seen")))

# Diagramm erstellen
ggplot(df_long_memory_filtered2, aes(x = name_co2e, y = relative_diff, color = treatment.group2)) +
  geom_point(size = 3, aes(shape = prev_seen)) +  
  geom_smooth(method = "lm", se = TRUE, aes(linetype = treatment.group2)) +  
  labs(
    title = "Deviations Between Subjective and Objective CO2e Values",
    x = "Food Items and Correct CO2e Values",
    y = "Subjective Deviations"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    legend.title = element_text(face = "bold"),
    legend.position = "top"
  ) +
  facet_wrap(~treatment.group2, scales = "free_y") +  
  scale_shape_manual(values = c(16, 17)) +  
  scale_color_brewer(palette = "Set1")  

#############################

# Vergleich zwischen geschätzten und tatsächlichen Werten (angelegt an Camilleri et al., 2018)
# Durchschnittswerte pro Lebensmittel berechnen
df_summary <- df_long_memory_filtered2 %>%
  group_by(name, treatment.group2, prev_seen) %>%  
  summarise(
    avg_log_correct = mean(log(correct + 1), na.rm = TRUE),
    avg_log_estimate = mean(log(estimate + 1), na.rm = TRUE),
    .groups = "drop"
  )

df_summary <- df_summary %>%
  mutate(prev_seen = as.factor(prev_seen))

ggplot(df_summary, aes(x = avg_log_correct, y = avg_log_estimate, color = treatment.group2, shape = prev_seen)) +
  geom_point(size = 3, alpha = 0.8) +  
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey") +  
  geom_text_repel(aes(label = name), size = 3, max.overlaps = Inf) +  
  labs(
    title = "Comparison of Estimated and Actual CO2e Emissions",
    x = "log(Actual CO2e Emissions)",
    y = "log(Estimated CO2e Emissions)",
    color = "Group",
    shape = "Known vs Unknown"  # Legendenbeschriftung für prev_seen
  ) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 10)
  ) +
  scale_color_manual(values = c("control" = "red", "experimental" = "blue"))  




#################################################################################################################################

# DESKRIPTIVE STATISTIK - Hypothesen 

# HYPOTHESE H1
# Prozentsätze nachhaltiger und nicht nachhaltiger Entscheidungen pro Gruppe
choice_percentages_by_group <- df_long_choice_filtered %>% 
  group_by(treatment.group2) %>% 
  summarise(
    count_sustainable = sum(sustainable.choice == 1, na.rm = TRUE),
    total_count = n(),
    Sustainable = (count_sustainable / total_count) * 100,
    Nonsustainable = 100 - Sustainable,   
    .groups = 'drop'
  ) %>% 
  pivot_longer(cols = c(Sustainable, Nonsustainable), names_to = "choice_type", values_to = "percentage")
print(choice_percentages_by_group)

# Create the stacked bar chart
ggplot(choice_percentages_by_group, aes(x = treatment.group2, y = percentage, fill = choice_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Sus. vs. Non-Sus. Choices by Exp. Group",
       x = "Treatment Group",
       y = "Percentage",
       fill = "Choice Type") +
  scale_fill_manual(values = c("Sustainable" = "#1f77b4", "Nonsustainable" = "#ff7f0e"))

# Summe der nachhaltigen Entscheidungen im Mittel pro Proband
df_avg_sustainableChoice_per_participant <- df_long_memory_filtered %>%
  mutate(treatment_group2 = ifelse(treatment.group == "control", "control", "experimental")) %>%
  group_by(participant.id, treatment_group2) %>%
  summarise(mean_sustainableChoice = mean(sum_sustainableChoice, na.rm = TRUE)) %>%
  group_by(treatment_group2) %>%
  summarise(avg_sustainableChoice_per_participant = mean(mean_sustainableChoice, na.rm = TRUE))
df_avg_sustainableChoice_per_participant

# HYPOTHESE H2
# Berechnung der Prozentsätze nachhaltiger und nicht nachhaltiger Entscheidungen für die beiden experimentellen Gruppen
choice_percentages_by_group <- df_long_choice_filtered %>% 
  filter(treatment.group %in% c("label", "norm")) %>%  # Nur label und norm Gruppen
  group_by(treatment.group) %>% 
  summarise(
    count_sustainable = sum(sustainable.choice == 1, na.rm = TRUE),
    count_nonsustainable = sum(sustainable.choice == 0, na.rm = TRUE),
    total_count = n(),
    Sustainable = (count_sustainable / total_count) * 100,
    Nonsustainable = (count_nonsustainable / total_count) * 100,
    .groups = 'drop'
  ) %>% 
  pivot_longer(cols = c(Sustainable, Nonsustainable), names_to = "choice_type", values_to = "percentage")
print(choice_percentages_by_group)

# Erstellen des gestapelten Balkendiagramms
ggplot(choice_percentages_by_group, aes(x = treatment.group, y = percentage, fill = choice_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Sus. vs. Non-Sus. Choices by Exp. Group",
       x = "Experimental Group",
       y = "Percentage",
       fill = "Choice Type") +
  scale_fill_manual(values = c("Sustainable" = "#1f77b4", "Nonsustainable" = "#ff7f0e"))

# Berechnung des Durchschnitts nachhaltiger Entscheidungen pro Proband für die experimentellen Gruppen
df_avg_sustainableChoice_per_participant <- df_long_memory_filtered %>%
  filter(treatment.group %in% c("label", "norm")) %>%  # Nur label und norm Gruppen
  group_by(participant.id, treatment.group) %>%
  summarise(mean_sustainableChoice = mean(sum_sustainableChoice, na.rm = TRUE)) %>%
  group_by(treatment.group) %>%
  summarise(avg_sustainableChoice_per_participant = mean(mean_sustainableChoice, na.rm = TRUE))
print(df_avg_sustainableChoice_per_participant)




#################################################################################################################################

# H1-H3: LOGISTISCHE MULTILEVEL-REGRESSION

# HYPOTHESE 1: Participants in both experimental groups will choose the sustainable option more frequently compared to the control group
mod_H1 <- glmer(sustainable.choice ~ scale(round.number) + treatment.group2 + (1 | participant.id) + (1 | round.stimuliID), 
              data = df_long_choice_filtered, family = binomial)
summary(mod_H1)
# Experimentelle Behandlungsansatz hat keinen signifikanten Einfluss auf die nachhaltige Wahl
# Effekt der Runde allein ist nicht signifikant

emm_H1 <- emmeans(mod_H1, ~ treatment.group2)
contrast(emm_H1, method = "pairwise")

#############################

# HYPOTHESE 2: The group receiving both labels and social norms will choose sustainable foods more frequently than the group receiving only labels
# Daten filtern, um nur die relevanten Gruppen zu erhalten
df_filtered <- df_long_choice_filtered %>%
  filter(treatment.group %in% c("norm", "label"))
df_filtered$participant.id <- as.factor(df_filtered$participant.id)

# Modell für die Hypothese 2 erstellen
mod_H2 <- glmer(sustainable.choice ~ treatment.group + scale(round.number) + 
                  (1 | participant.id) + (1 | round.stimuliID), 
                data = df_filtered, 
                family = binomial)
summary(mod_H2)

#############################

# HYPOTHESE 3: Individuals with higher pro-environmental attitudes will choose the sustainable option more frequently
# Modell 3: Hinzufügen der Pro-Environmental Attitudes und CO2-Emissionen als Kontrollvariablen
mod_H3 <- glmer(sustainable.choice ~ treatment.group2 + scale(sustainability_score) + 
                  scale(round.number) + scale(price.diff) + scale(CO2.diff) + scale(protein.diff) +  
                  (1 | participant.id) + (1 | round.stimuliID), 
                data = df_long_choice_filtered, 
                family = binomial)
summary(mod_H3)

# participant.id: signifikante Unterschiede zwischen den Teilnehmern
# round.stimuliID: Unterschiede zwischen den Stimuli 
# höhere Nachhaltigkeitswerte korrelieren mit einer höheren Wahrscheinlichkeit für nachhaltige Entscheidungen

#############################

# MODERATORANALYSE

# Modell 4: Interaktion zwischen Gruppenzugehörigkeit (label und norm) und Pro-Environmental Attitudes
# Untersucht, ob die Pro-Umwelt-Einstellungen den Einfluss der Gruppenzugehörigkeit auf die Entscheidung zur Nachhaltigkeit moderieren

mod4 <- glmer(sustainable.choice ~ treatment.group2 * scale(sustainability_score) + 
                scale(round.number) + scale(price.diff) + scale(CO2.diff) + scale(protein.diff) + 
                (1 | participant.id) + (1 | round.stimuliID), 
              data = df_long_choice_filtered, 
              family = binomial,
              control = glmerControl(optimizer = "bobyqa"))
summary(mod4)
# sustainability_score signifikant: ein höherer Nachhaltigkeitswert ist mit einer höheren Wahrscheinlichkeit verbunden, dass eine nachhaltige Wahl getroffen wird
# Alle anderen Variablen sind nicht signifikant, Interaktion auch nicht 

# Visualization
ggplot(df_long_choice_filtered, aes(x = sustainability_score, y = sustainable.choice, color = treatment.group2)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Interaction between Group Membership and Pro-Environmental Attitudes", 
       x = "Pro-Environmental Attitudes", 
       y = "Probability of Sustainable Decision")

# Kontrastanalyse
# Unterschiede zwischen den Gruppen (Interventionsgruppen vs. Kontrollgruppe und Label vs. Norm) hinsichtlich ihrer Umwelteinstellungen und deren Einfluss auf nachhaltige Entscheidungen
emm <- emmeans(mod4, ~ treatment.group2 | scale(sustainability_score))
contrast(emm, method = "pairwise", by = "sustainability_score", adjust = "bonferroni")

# Visualisierung der Kontraste
# Wahrscheinlichkeit, dass Teilnehmer eine nachhaltige Wahl treffen, in Abhängigkeit von der Behandlungsgruppe (Kontrollgruppe vs. Experimentalgruppe)
emm_df <- as.data.frame(emm)
emm_df$probability <- plogis(emm_df$emmean) 

# Konfidenzintervalle berechnen
emm_df$lower_ci <- plogis(emm_df$emmean - (1.96 * emm_df$SE))  # Unteres CI
emm_df$upper_ci <- plogis(emm_df$emmean + (1.96 * emm_df$SE))  # Oberes CI

# Plot erstellen
ggplot(emm_df, aes(x = treatment.group2, y = probability, color = treatment.group2)) + 
  geom_point(size = 3) + 
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) + 
  labs(title = "Probability of Sustainable Choice by Treatment Group", 
       x = "Treatment Group", 
       y = "Probability of Sustainable Decision")

#############################

# HYPOTHESE 4 - MULTILEVEL REGRESSION ANALYSIS: RECALL ACCURACY
# Multilevel-Regression für Recall Accuracy abhängig davon, ob das Carbon Attribut gesehen wurde sowie von den Umwelt-Werten
# Grundmodell ohne carbon_viewed
# Erstellen der neuen Gruppierungsvariable 'treatment.group2'
df_long_memory_filtered2$treatment.group2 <- ifelse(df_long_memory_filtered2$treatment.group %in% c("norm", "label"), 
                                                    "experimental", 
                                                    "control")

model_recall_step1 <- lmer(relative_diff ~ treatment.group2 + scale(sustainability_score) + prev_seen + 
                             (1 | participant.id) + (1 | round.number), 
                           data = df_long_memory_filtered2)
summary(model_recall_step1)

# Modell mit carbon_viewed als festem Effekt
model_recall_step2 <- lmer(relative_diff ~ treatment.group2 + scale(sustainability_score) + carbon_viewed + prev_seen + 
                             (1 | participant.id) + (1 | round.number), 
                           data = df_long_memory_filtered2,
                           control = lmerControl(optimizer = "bobyqa"))
summary(model_recall_step2)

##########

# Visualisierung je nachdem ob es sich um bekannte oder unbekannte CO2e Attribute handelt (erste 5 Runden vs. letzte zwei)
# Gruppierung nach prev_seen und Treatment-Gruppe
prev_seen_group_summary <- df_long_memory_filtered2 %>%
  group_by(prev_seen, treatment.group2) %>%
  summarise(
    mean_relative_diff = mean(relative_diff, na.rm = TRUE),
    se = sd(relative_diff, na.rm = TRUE) / sqrt(n()),
    ci_lower = mean_relative_diff - 1.96 * se,
    ci_upper = mean_relative_diff + 1.96 * se
  )

# Conversion von `prev_seen` zu einem Faktor mit Labels
prev_seen_group_summary$prev_seen <- factor(prev_seen_group_summary$prev_seen, 
                                            levels = c(0, 1), 
                                            labels = c("Unknown Attribute (Round 6-7", "Known Attribute (Round 1-5)"))

# Plotting
ggplot(prev_seen_group_summary, aes(x = treatment.group2, y = mean_relative_diff, fill = prev_seen)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                position = position_dodge(0.9), width = 0.2) +
  labs(
    title = "Recall Accuracy by Treatment Group and Round",
    x = "Treatment Group",
    y = "Mean Relative Difference",
    fill = "Carbon Attribute"
  ) +
  scale_fill_manual(values = c("yellow", "lightgreen"))


##########

# Visualisierung je nachdem ob das CO2 Attribut angesehen wurde oder nicht 
effect_summary <- df_long_memory_filtered2 %>%
  group_by(treatment.group2, carbon_viewed) %>%
  summarise(mean_relative_diff = mean(relative_diff, na.rm = TRUE),
            se = sd(relative_diff, na.rm = TRUE) / sqrt(n()),  
            ci_lower = mean_relative_diff - 1.96 * se,
            ci_upper = mean_relative_diff + 1.96 * se,
            .groups = 'drop')

effect_summary$carbon_viewed <- factor(effect_summary$carbon_viewed, 
                                       levels = c(0, 1), 
                                       labels = c("Not Viewed", "Viewed"))

# Plotting
ggplot(effect_summary, aes(x = treatment.group2, y = mean_relative_diff, fill = carbon_viewed)) +
  geom_bar(stat = "identity", position = position_dodge()) +  
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                position = position_dodge(0.9), width = 0.2) +
  labs(title = "Recall Accuracy by Treatment Group and Viewing of Carbon Attribute",
       x = "Treatment Group",
       y = "Mean Relative Difference") +
  scale_fill_manual(values = c("grey", "skyblue"), 
                    name = "Carbon Attribute")

#############################

# Hier noch mit carbon_viewed als Interaktionseffekt
# Interaktioneffekte: Gruppenzugehörigkeit und Ansehen des Carbon Attributs auf Recall Accuracy 
model_recall_interaction <- lmer(relative_diff ~ treatment.group2 * carbon_viewed + 
                                   scale(sustainability_score) + 
                                   (1 | participant.id) + (1 | round.number), 
                                 data = df_long_memory_filtered2)
summary(model_recall_interaction)

# Kontraste zur Untersuchung der Gruppenunterschiede
emm_options(pbkrtest.limit = 7400)
emm <- emmeans(model_recall_interaction, ~ treatment.group2 * carbon_viewed, infer = TRUE)
emm_summary <- summary(emm)

# Visualisierung der Ergebnisse
ggplot(emm_summary, aes(x = treatment.group2, y = emmean, fill = as.factor(carbon_viewed))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Effect of Treatment Group on Recall Accuracy",
       x = "Treatment Group",
       y = "Recall Accuracy",
       fill = "Carbon Attribute Viewed")




#################################################################################################################################

# SEKUNDÄRE ANALYSEN 

# 1. Recall Performance
# Analyse der Recall-Performance in Abhängigkeit von den Umwelteinstellungen

model_recall2 <- lmer(relative_diff ~ sustainability_score + treatment.group2 + 
                        (1 | participant.id) + (1 |round.number),
                      data = df_long_memory_filtered2)
summary(model_recall2)

#############################

# 2. Impact of Pro-environmental Attitudes
# Vergleich der Recall-Leistung basierend auf den Umweltwerten
model_pro_env <- lm(relative_diff ~ sustainability_score, data = df_long_memory_filtered2)
summary(model_pro_env)

# Visualisierung der Recall-Leistung in Abhängigkeit von den Umweltwerten
ggplot(df_long_memory_filtered2, aes(x = sustainability_score, y = relative_diff)) +
  geom_point(aes(color = treatment.group)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Recall Performance vs. Pro-Environmental Attitudes", 
       x = "Pro-Environmental Attitudes", 
       y = "Recall Performance (Absolute Difference)")

#############################

# 3. Effect of Interventions
# Analyse der Auswirkungen der Interventionen auf die Betrachtung von Umweltattributen
tracking_data_intervention <- df_long_choice_filtered %>%
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
  data = df_long_memory_filtered2,
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
df_long_choice_filtered$participant.id <- as.factor(df_long_choice_filtered$participant.id)
first_viewed$participant.id <- as.factor(first_viewed$participant.id)
last_viewed$participant.id <- as.factor(last_viewed$participant.id)

# Mergen der neuen Variablen mit dem Hauptdatensatz
df_long_choice_filtered <- df_long_choice_filtered %>%
  left_join(first_viewed, by = c("participant.id", "round.number")) %>%
  left_join(last_viewed, by = c("participant.id", "round.number"))

#############################

# Explorative Analyse: Suchen von Mustern in der Informationssuche und deren Einfluss auf Essensentscheidungen
model_choice_patterns <- glmer(sustainable.choice ~ first_attribute_viewed + last_attribute_viewed + (1 | participant.id), 
                               data = df_long_choice_filtered, 
                               family = binomial, 
                               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(model_choice_patterns)

# Einfluss dessen, ob in der vorherigen Rund das CO2 Attribut zuletzt gesehen wurde darauf, ob in der nächsten Runde vermehrt die Nachhaltige Wahl getroffen wurde
df_long_choice_filtered <- df_long_choice_filtered %>%
  mutate(previous_CO2_viewed = lag(ifelse(last_attribute_viewed == "B_co2e/kg", 1, 0), order_by = round.number))

model_CO2_effect <- glmer(sustainable.choice ~ previous_CO2_viewed + round.number + 
                            (1 | participant.id), 
                          data = df_long_choice_filtered, 
                          family = binomial, 
                          control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(model_CO2_effect)
# previous_CO2_viewed: Positiver Schätzwert aber nicht signfikant
# Leichter Anstieg in der Wahrscheinlichkeit, eine nachhaltige Wahl zu treffen, wenn das CO2-Attribut in der vorherigen Runde zuletzt angesehen wurde

#############################

# 5. Robustness Check
# Deskriptiv
# Teilnehmer, die den ersten Verständnischeck nicht bestanden haben
failed_first_attempt <- df_wide_choice_filtered %>% filter(comp.check1 == 0)

# Teilnehmer, die den zweiten Verständnischeck (falls vorhanden) ebenfalls nicht bestanden haben
failed_second_attempt <- failed_first_attempt %>% filter(comp.check2 == 0)

# Teilnehmer, die den ersten Check bestanden haben (zur Validierung)
passed_first_attempt <- df_wide_choice_filtered %>% filter(comp.check1 == 1)

# Ausgabe der Anzahl der Teilnehmer
cat("Anzahl der Teilnehmer, die den ersten Verständnischeck nicht bestanden haben:", nrow(failed_first_attempt), "\n")
# 33 
cat("Anzahl der Teilnehmer, die den ersten und zweiten Verständnischeck nicht bestanden haben:", nrow(failed_second_attempt), "\n")
# 9
cat("Anzahl der Teilnehmer, die den ersten Verständnischeck bestanden haben:", nrow(passed_first_attempt), "\n")
# 330


# Veganer und Vegetarier ausschliessen
# Filter für Allesesser
df_long_choice_filtered_nonveg <- df_long_choice_filtered %>% 
  filter(participant.id %in% df_wide_choice_filtered$participant.id[df_wide_choice_filtered$num_ernährung == 1])

# HYPOTHESE 1: Experimentelle Gruppen vs. Kontrollgruppe
mod_H1_nonveg <- glmer(sustainable.choice ~ scale(round.number) + treatment.group2 + 
                         (1 | participant.id) + (1 | round.stimuliID), 
                       data = df_long_choice_filtered_nonveg, 
                       family = binomial)
summary(mod_H1_nonveg)

emm_H1_nonveg <- emmeans(mod_H1_nonveg, ~ treatment.group2)
contrast(emm_H1_nonveg, method = "pairwise")

# HYPOTHESE 2: Norm vs. Label
df_filtered_nonveg <- df_long_choice_filtered_nonveg %>% 
  filter(treatment.group %in% c("norm", "label"))

mod_H2_nonveg <- glmer(sustainable.choice ~ treatment.group + scale(round.number) + 
                         (1 | participant.id) + (1 | round.stimuliID), 
                       data = df_filtered_nonveg, 
                       family = binomial)
summary(mod_H2_nonveg)

# HYPOTHESE 3: Nachhaltigkeitswerte und CO2
mod_H3_nonveg <- glmer(sustainable.choice ~ treatment.group2 + scale(sustainability_score) + 
                         scale(round.number) + scale(price.diff) + scale(CO2.diff) + 
                         scale(protein.diff) + 
                         (1 | participant.id) + (1 | round.stimuliID), 
                       data = df_long_choice_filtered_nonveg, 
                       family = binomial)
summary(mod_H3_nonveg)

# MODERATORANALYSE: Interaktion Nachhaltigkeitswerte und Gruppenzugehörigkeit
mod4_nonveg <- glmer(sustainable.choice ~ treatment.group2 * scale(sustainability_score) + 
                       scale(round.number) + scale(price.diff) + scale(CO2.diff) + 
                       scale(protein.diff) + 
                       (1 | participant.id) + (1 | round.stimuliID), 
                     data = df_long_choice_filtered_nonveg, 
                     family = binomial, 
                     control = glmerControl(optimizer = "bobyqa"))
summary(mod4_nonveg)




#################################################################################################################################

# PRÜFUNG DER VORAUSSETZUNGEN FÜR DIE MODELLE

# Multikollinearität (VIF) für alle Prädiktoren im Modell
vif_results <- vif(mod_H3)
print(vif_results)
# Kein Hinweis auf Multikollinearität

#############################

# Hierarchische Struktur und Intraklassenkorrelation (ICC mit einem Nullmodell)
null_model <- glmer(sustainable.choice ~ 1 + (1 | participant.id), data = df_long_choice_filtered, family = binomial)
full_model <- glmer(sustainable.choice ~ round.number + treatment.group + (1 | participant.id), data = df_long_choice_filtered, family = binomial)

var_null_model <- as.data.frame(VarCorr(null_model))$vcov[1]
var_full_model_random <- as.data.frame(VarCorr(full_model))$vcov[1]  
var_full_model_residual <- sigma(full_model)^2 

icc <- var_full_model_random / (var_full_model_random + var_full_model_residual)
print(icc)
# ICC von 0.468, 47% der Gesamtvarianz in der abhängigen Variablen (in diesem Fall sustainable.choice) wird durch Unterschiede zwischen den Teilnehmern erklärt 

#############################

# Homoskedastizität und Residuenanalyse
standardized_residuals_full <- residuals(full_model, type = "pearson")
plot(fitted(full_model), standardized_residuals_full, 
     xlab = "Fitted Values", ylab = "Standardized Residuals", main = "Residuals vs Fitted (Full Model)")
abline(h = 0, col = "red")


# Standardisierte Residuen berechnen
standardized_residuals_full <- residuals(full_model, type = "pearson")

# Erstellen einer zusammenfassenden Tabelle für die Residuen
residual_summary <- data.frame(
  Mean = mean(residuals(full_model)),  # Verwende full_model hier
  SD = sd(residuals(full_model)),      # Verwende full_model hier
  Min = min(residuals(full_model)),    # Verwende full_model hier
  Max = max(residuals(full_model)),    # Verwende full_model hier
  N = length(residuals(full_model)),   # Verwende full_model hier
  Outliers = sum(abs(residuals(full_model)) > 2 * sd(residuals(full_model)))  # Verwende full_model hier
)

# Tabelle ausgeben
print(residual_summary)


#############################

# Lineare Beziehung auf der Logit-Skala (Box-Tidwell-Test)
boxTidwell(sustainable.choice ~ round.number + treatment.group + 
             num_geschlecht + num_studierende + num_einkauf + 
             num_politische_orientierung + num_einkommen,  
           data = df_wide_choice_filtered)

# Box-Tidwell-Test für df_long_memory_filtered2
boxTidwell(sustainable.option ~ round.number + treatment.group + sustainability_score + combined_var, 
           data = df_long_memory_filtered2)

# Box-Tidwell-Test für df_tracking
boxTidwell(sustainable.choice ~ round.number + treatment.group + combined_var + treatment.group2, 
           data = df_tracking)

#############################

# Modell-Güte (Hosmer-Lemeshow-Test)
library(ResourceSelection)
hoslem_test <- hoslem.test(df$sustainable.choice, predicted_probs, g = 10)


