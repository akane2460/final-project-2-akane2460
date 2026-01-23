## Load Packages ----
library(tidyverse)
library(skimr)
library(janitor)
library(here)
library(knitr)
library(corrplot)

# load cleaned data
load(here("data/diabetic_clean.rda"))

# Medication Risk Index---
  # based on previous models, medication regimen data improves risk assessment
  # here, identify the medications and the associated regimen changes that 
  # signal greatest risk for readmission

general_readmit_rates <- diabetic_clean |>
  group_by(readmitted) |> 
  summarize(
    n = n(),
    readmit_rate = n()/96921,
    .groups = "drop"
  )

# cols <- c("4" = "#242806", "6" = "#F7FADB", "8" = "#143D6C")

# '4CB07E', '4CB0B0', 'B04CB0'

cols <- c("Not Readmitted" = "#4CB04C", "Readmitted" = "#B04C4C")


readmit_rates_plot <- general_readmit_rates |>
  mutate(readmitted = recode(readmitted,
                             NO = "Not Readmitted",
                             YES = "Readmitted")) |> 
  ggplot(aes(x = readmitted, y = readmit_rate, fill = readmitted)) +
  geom_col() +
  geom_text(
            aes(
            x = readmitted,
            y = readmit_rate,
            label = scales::percent(readmit_rate, accuracy = 0.1)),
    vjust = -0.5,
    size = 4
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = cols) +
  labs(
    title = "30-Day Hospital Readmission Rates",
    x = "Readmission Status",
    y = "30-Day Readmission Rate",
  ) +
  guides(fill = "none") +
  theme_minimal()

ggsave("additional_analyses/plots/readmit_rates_plot.png", plot = readmit_rates_plot)





## Medication Validation Exploration----
### insulin

## Medication Risk Summary ----
# medications of interest 
meds <- c("insulin", "rosiglitazone", "pioglitazone", "glyburide", "glipizide", "glimepiride", 
          "metformin")

med_readmit_summary <- diabetic_clean |>
  select(readmitted, all_of(meds)) |>
  pivot_longer(
    cols = all_of(meds),
    names_to = "medication",
    values_to = "status"
  ) |>
  group_by(medication, status) |>
  summarize(
    n = n(),
    readmit_rate = mean(readmitted == "YES", na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(readmit_rate = round(readmit_rate, 3))

med_readmit_wide <- med_readmit_summary |>
  select(medication, status, readmit_rate) |>
  pivot_wider(
    names_from = status,
    values_from = readmit_rate
  )

med_readmit_wide |> knitr::kable()

# medication       No  Down Steady    Up
# <chr>         <dbl> <dbl>  <dbl> <dbl>
#   1 glimepiride   0.464 0.481  0.477 0.429
# 2 glipizide     0.461 0.529  0.487 0.499
# 3 glyburide     0.465 0.492  0.459 0.449
# 4 insulin       0.44  0.53   0.455 0.517
# 5 metformin     0.471 0.458  0.436 0.412
# 6 pioglitazone  0.463 0.526  0.485 0.518
# 7 rosiglitazone 0.463 0.329  0.492 0.397


  # steady dosing red flags:
    # rosiglitazone
  # changed dosing red flags:
    # insulin
    # pioglitazone 
    # glipizide

    
## Medication Risk Plots ----

### glimepiride
diabetic_clean |>
  group_by(glimepiride) |>
  summarize(
    readmit_rate = mean(readmitted == "YES", na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  ggplot(aes(x = glimepiride, y = readmit_rate)) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(label = scales::percent(readmit_rate, accuracy = 0.1)),
    vjust = -0.5,
    size = 4
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "30-Day Hospital Readmission Rate by Inpatient Glimepiride Prescribing and Dose Changes",
    x = "Glimepiride Status",
    y = "30-Day Readmission Rate"
  ) +
  theme_minimal()

### glipizide
diabetic_clean |>
  group_by(glipizide) |>
  summarize(
    readmit_rate = mean(readmitted == "YES", na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  ggplot(aes(x = glipizide, readmit_rate, y = readmit_rate)) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(label = scales::percent(readmit_rate, accuracy = 0.1)),
    vjust = -0.5,
    size = 4
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "30-Day Hospital Readmission Rate by Inpatient Glipizide Prescribing and Dose Changes",
    x = "Glipizide Status",
    y = "30-Day Readmission Rate"
  ) +
  theme_minimal()

### glyburide
diabetic_clean |>
  group_by(glyburide) |>
  summarize(
    readmit_rate = mean(readmitted == "YES", na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  ggplot(aes(x = glyburide, readmit_rate, y = readmit_rate)) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(label = scales::percent(readmit_rate, accuracy = 0.1)),
    vjust = -0.5,
    size = 4
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "30-Day Hospital Readmission Rate by Inpatient Glyburide Prescribing and Dose Changes",
    x = "Glyburide Status",
    y = "30-Day Readmission Rate"
  ) +
  theme_minimal()

### insulin
diabetic_clean |>
  group_by(insulin) |>
  summarize(
    readmit_rate = mean(readmitted == "YES", na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  ggplot(aes(x = insulin, readmit_rate, y = readmit_rate)) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(label = scales::percent(readmit_rate, accuracy = 0.1)),
    vjust = -0.5,
    size = 4
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "30-Day Hospital Readmission Rate by Inpatient Insulin Prescribing and Dose Changes",
    x = "Insulin Status",
    y = "30-Day Readmission Rate"
  ) +
  theme_minimal()
### metformin
diabetic_clean |>
  group_by(metformin) |>
  summarize(
    readmit_rate = mean(readmitted == "YES", na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  ggplot(aes(x = metformin, readmit_rate, y = readmit_rate)) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(label = scales::percent(readmit_rate, accuracy = 0.1)),
    vjust = -0.5,
    size = 4
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "30-Day Hospital Readmission Rate by Inpatient Metformin Prescribing and Dose Changes",
    x = "Metformin Status",
    y = "30-Day Readmission Rate"
  ) +
  theme_minimal()

### pioglitazone
diabetic_clean |>
  group_by(pioglitazone) |>
  summarize(
    readmit_rate = mean(readmitted == "YES", na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  ggplot(aes(x = pioglitazone, readmit_rate, y = readmit_rate)) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(label = scales::percent(readmit_rate, accuracy = 0.1)),
    vjust = -0.5,
    size = 4
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "30-Day Hospital Readmission Rate by Inpatient Pioglitazone Prescribing and Dose Changes",
    x = "Pioglitazone Status",
    y = "30-Day Readmission Rate"
  ) +
  theme_minimal()

### rosiglitazone
diabetic_clean |>
  group_by(rosiglitazone) |>
  summarize(
    readmit_rate = mean(readmitted == "YES", na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  ggplot(aes(x = rosiglitazone, readmit_rate, y = readmit_rate)) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(label = scales::percent(readmit_rate, accuracy = 0.1)),
    vjust = -0.5,
    size = 4
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "30-Day Hospital Readmission Rate by Inpatient Rosiglitazone Prescribing and Dose Changes",
    x = "Rosiglitazone Status",
    y = "30-Day Readmission Rate"
  ) +
  theme_minimal()



## Multiple Medication Risk Assessment----
# medications of interest
meds <- c("insulin", "rosiglitazone", "pioglitazone", "glyburide", "glipizide", "glimepiride", 
          "metformin")

# select for patients on multiple medications from this list
n_meds_df <- diabetic_clean |> 
  mutate(across(all_of(meds),
                ~ . != "No",
                .names = "on_{col}"))

# create n_meds column
n_meds_df <- n_meds_df |> 
  mutate(n_meds = rowSums(across(starts_with("on_"), as.integer)))

# assess risk of # of medications on
n_meds_df |> 
  group_by(n_meds) |>
  summarize(
    n = n(),
    readmit_rate = mean(readmitted == "YES", na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(readmit_rate = round(readmit_rate, 3))
  # being on 2 medications most at risk for readmission
    # could indicate undercontrolled symptoms or adverse side effects
    # of 2 medications together


## med x med interactions ----
# create df for patients on 2 medications (from our list)
double_meds_df <- n_meds_df |> 
  filter(n_meds == 2)

# determine the most common medication combinations for patients to be on
double_meds_df |> 
  pivot_longer(
    cols = starts_with("on_"),
    names_to = "medication",
    values_to = "on_med"
  ) |> 
  filter(on_med == TRUE) |> 
  distinct(patient_nbr, medication) |> # ensure distinct patient number and medication (for hospital stay)
  group_by(patient_nbr) |>   
  summarize(
    med_pair = paste(sort(medication), collapse = " + "),
    .groups = "drop"
  ) |> 
  group_by(med_pair) |> 
  summarize(
    n_patients = n(),   # number of rows = number of patients
    .groups = "drop"
  ) |> 
  arrange(desc(n_patients))

# most patients are on 2 medications, typically insulin
# or metformin with some other drug
# so we're going to assess insulin and metformin interactions
# across entire medication suite
# most clinically used meds in general

## building med x med risk grids for insulin and metformin
double_meds_df <- double_meds_df |> 
  mutate(
    across(
      all_of(meds),
      ~ if_else(.x == "Steady", "Steady", "Changed"),
      .names = "{.col}_status"
    )
  )

# build med grid function
med_risk_grid_fxn <- function (df, base_med, test_med) {
  base_med_status <- paste0(base_med, "_status") # search for the med col
  test_med_status <- paste0(test_med, "_status") # search for the med col
  df |> 
    group_by(.data[[base_med_status]], .data[[test_med_status]]) |> 
    summarise(
      n = n(),
      readmit_rate = mean(readmitted == "YES", na.rm = TRUE),
      .groups = "drop"
    ) |> 
    mutate(medication = test_med)
}

# insulin grids
insulin_pioglitazone <- med_risk_grid_fxn(double_meds_df, "insulin", "pioglitazone")
insulin_glyburide <- med_risk_grid_fxn(double_meds_df, "insulin", "glyburide")
insulin_glipizide <- med_risk_grid_fxn(double_meds_df, "insulin", "glipizide")
insulin_glimepiride <- med_risk_grid_fxn(double_meds_df, "insulin", "glimepiride")
insulin_rosiglitazone <- med_risk_grid_fxn(double_meds_df, "insulin", "rosiglitazone")
insulin_metformin <- med_risk_grid_fxn(double_meds_df, "insulin", "metformin")
# metformin grids
metformin_pioglitazone <- med_risk_grid_fxn(double_meds_df, "metformin", "pioglitazone")
metformin_glyburide <- med_risk_grid_fxn(double_meds_df, "metformin", "glyburide")
metformin_glipizide <- med_risk_grid_fxn(double_meds_df, "metformin", "glipizide")
metformin_glimepiride <- med_risk_grid_fxn(double_meds_df, "metformin", "glimepiride")
metformin_rosiglitazone <- med_risk_grid_fxn(double_meds_df, "metformin", "rosiglitazone")
  
  # overall:
    # insulin x med red flags:
        #insulin x glyburide: both changed incr risk
        #insulin x glimepiride: both steady incr risk (changed also high)
        #insulin x metformin: both change BIG incr risk
    # metformin x med red flags:
        #metformin x glyburide: both changed incr risk ()

# Building a Medication Instability Index----

# MII

## single meds----
# building risk assessment function
single_med_risk_assessment_fxn <- function (med, condition, df) {
  df |> 
    filter(medication == med) |> 
    mutate(
      single_risk = ifelse(condition == 'Steady', 
                           (Steady - ((Up + Down)/2))/Steady, 
                           (((Up + Down)/2) - Steady)/((Up + Down)/2)) ) |> 
    pull(single_risk)
  
}

# prepare data for input into risk assessment function
medication_1 <- c("rosiglitazone", "insulin", "insulin",
                 "pioglitazone", "pioglitazone", "glipizide", 
                 "glipizide")
regimen_1 <- c("Steady", "Up", "Down", "Up", "Down", 
                     "Up", "Down")

single_med_red_flags <- data.frame(medication_1, regimen_1)

med_readmit_wide <- as.data.frame(med_readmit_wide)

# apply single med risk assessment function
single_med_red_flags <- single_med_red_flags  |> 
  rowwise() |> 
  mutate(
    singlerisk = single_med_risk_assessment_fxn(medication_1, regimen_1, med_readmit_wide)
  ) |> ungroup()

# double meds----
    #insulin x glyburide: both changed incr risk
    #insulin x glimepiride: both steady incr risk (changed also high)
    #insulin x metformin: both change BIG incr risk
    # metformin x med red flags:
    #metformin x glyburide: both changed incr risk ()

medication_1 <- c("insulin", "insulin", "insulin", "metformin")
medication_2 <- c("glyburide", "glimepiride", "metformin", "glyburide")
regimen_1 <- c("Changed", "Steady", "Changed", "Changed")
regimen_2 <- c("Changed", "Steady", "Changed", "Changed")


interaction_matrices <- list(
  insulin_glyburide    = insulin_glyburide,
  insulin_glimepiride  = insulin_glimepiride,
  insulin_metformin   = insulin_metformin,
  metformin_glyburide = metformin_glyburide
)

double_med_red_flags <- data.frame(medication_1, medication_2, regimen_1, regimen_2)

double_med_risk_fxn <- function(med1, med2, regimen1, regimen2, interaction_list) {
  
  key <- paste(med1, med2, sep = "_")
  
  if (!key %in% names(interaction_list)) {
    stop(paste("No interaction matrix found for", key))
  }
  
  df <- interaction_list[[key]]
  
  med1_status <- paste0(med1, "_status")
  med2_status <- paste0(med2, "_status")
  
  # safer comparison (opposite extreme)
  safer1 <- ifelse(regimen1 == "Changed", "Steady", "Changed")
  safer2 <- ifelse(regimen2 == "Changed", "Steady", "Changed")
  
  risky_rate <- df |>
    filter(
      .data[[med1_status]] == regimen1,
      .data[[med2_status]] == regimen2
    ) |>
    pull(readmit_rate)
  
  safer_rate <- df |>
    filter(
      .data[[med1_status]] == safer1,
      .data[[med2_status]] == safer2
    ) |>
    pull(readmit_rate)
  
   double_med_risk_score <- (risky_rate - safer_rate) / safer_rate
}


double_med_red_flags <- double_med_red_flags |>
  mutate(
    double_med_risk_score = pmap_dbl(
      list(medication_1, medication_2, regimen_1, regimen_2),
      double_med_risk_fxn,
      interaction_list = interaction_matrices
    )
  )


## building single and double risk----
single_meds <- unique(single_med_red_flags$medication_1)

patient_single_med_long <- diabetic_clean |>
  select(encounter_id, readmitted, all_of(single_meds)) |>
  pivot_longer(
    cols = all_of(single_meds),
    names_to = "medication_1",
    values_to = "regimen_1"
  )

patient_single_med_scored <- patient_single_med_long |>
  left_join(
    single_med_red_flags,
    by = c("medication_1", "regimen_1")
  ) |>
  mutate(
    singlerisk = replace_na(singlerisk, 0)
  )

patient_single_med_scored <- patient_single_med_scored |>
  group_by(encounter_id) |>
  summarise(
    single_med_risk_score = sum(singlerisk),
    .groups = "drop"
  )
  
diabetic_clean <- diabetic_clean |>
  left_join(patient_single_med_scored, by = "encounter_id") 

### double meds risk implementation
# create med1 x med2 pairings

flagged_meds <- unique(c(
  double_med_red_flags$medication_1,
  double_med_red_flags$medication_2
))


 


patient_double_long <- diabetic_clean |>
  pivot_longer(
    cols = all_of(unique(c(double_med_red_flags$medication_1, double_med_red_flags$medication_2))),
    names_to = "medication",
    values_to = "regimen"
  ) |>
  filter(regimen != "No") |>   # drop meds the patient isn’t on
  mutate(risk_regimen = ifelse(regimen %in% c("Up", "Down"), "Changed", regimen))


patient_double_risk <- diabetic_clean |>
  select(encounter_id) |>
  mutate(double_med_risk_score = 0)

for(i in seq_len(nrow(double_med_red_flags))) {
  
  med1 <- double_med_red_flags$medication_1[i]
  med2 <- double_med_red_flags$medication_2[i]
  reg1 <- double_med_red_flags$regimen_1[i]
  reg2 <- double_med_red_flags$regimen_2[i]
  risk <- double_med_red_flags$double_med_risk_score[i]
  
  # find patients who match this pair & regimen
  patients_with_risk <- patient_double_long |>
    filter(medication == med1 & risk_regimen == reg1) |>
    inner_join(
      patient_double_long |> filter(medication == med2 & risk_regimen == reg2),
      by = "encounter_id"
    ) |>
    pull(encounter_id)
  
  # add risk
  patient_double_risk <- patient_double_risk |>
    mutate(double_med_risk_score = ifelse(encounter_id %in% patients_with_risk,
                                double_med_risk_score + risk,
                                double_med_risk_score))
}

patient_double_risk_agg <- patient_double_risk |>
  group_by(encounter_id) |>
  summarise(double_med_risk_score = sum(double_med_risk_score, na.rm = TRUE), .groups = "drop")

diabetic_clean <- diabetic_clean |>
  left_join(patient_double_risk_agg, by = "encounter_id")

## create total risk index----
diabetic_mii <- 
  diabetic_clean |> 
  mutate(medication_instability_index = double_med_risk_score + single_med_risk_score)

## save data with risk index included
save(diabetic_mii, file = here("data/diabetic_mii.rda"))

## OVERALL----
# change regimen red flags
  # glipizide
  # insulin
  # piogliatzone
# steady regimen red flags
  # rosigliatzone

