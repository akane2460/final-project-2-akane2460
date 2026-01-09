## Load Packages ----
library(tidyverse)
library(skimr)
library(janitor)
library(here)
library(knitr)
library(corrplot)

# load training data 
load(here("data/diabetic_clean.rda"))
# load cleaned data
load(here("data/diabetic_clean.rda"))


# Medication Risk Index---
  # based on previous models, medication regimen data improves risk assessment
  # here, identify the medications and the associated regimen changes that 
  # signal greatest risk for readmission

## Medication Validation Exploration----
### insulin


## Medication Risk Summary ----
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


## OVERALL----
# change regimen red flags
  # glipizide
  # insulin
  # piogliatzone
# steady regimen red flags
  # rosigliatzone

