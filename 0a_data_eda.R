## Load Packages ----

library(tidyverse)
library(skimr)
library(janitor)
library(here)
library(knitr)

# read in data----
diabetic_data <- read_csv(here("data/diabetic_data.csv"))

# clean names----
diabetic_data <- diabetic_data |> clean_names()

# write out shortened version of data for memo 1----
shortened_diabetic_data <- diabetic_data |> 
  head()

shortened_diabetic_data <- kable(shortened_diabetic_data, format = "html")

file_path_shortened_diabetic_data <- "memos/results/shortened_diabetic_data.html"

writeLines(as.character(shortened_diabetic_data), con = file_path_shortened_diabetic_data)

# quality check----
diabetic_data |> skimr::skim_without_charts()

# examining "?" values----

# they placed ? in place of NA for missing values
diabetic_data |> filter(weight == "?")
diabetic_data |> filter(weight != "?")
# lots of "missing" data for weight

diabetic_data |> filter(race == "?")
diabetic_data |> filter(race != "?")
# some missing data for race

diabetic_data |> filter(payer_code == "?")
diabetic_data |> filter(payer_code != "?")
# lots of missigness for payer_code

diabetic_data |> filter(medical_specialty == "?")
diabetic_data |> filter(medical_specialty != "?")
# lots of missigness for medical_specialty

diabetic_data |> filter(diag_1 == "?")
diabetic_data |> filter(diag_1 != "?")
# very little missign for diag_1

diabetic_data |> filter(diag_2 == "?")
diabetic_data |> filter(diag_2 != "?")
# very little missign for diag_2

diabetic_data |> filter(diag_3 == "?")
diabetic_data |> filter(diag_3 != "?")
# very little missign for diag_3 (slightly more than 1 and 2)

# logic checking weight----

# about 50 patients have weight in the range of 0 - 25 pounds
# if these patients are not children, that is impossible
# weight has substantial issues like this in the rest of the dataset
# ie. is it possible for a 45 year old to weigh between 50 to 75 pounds?
# no not likely
# 
# for this reason, weight is not going to be considered in the analysis
# and will be excluded from the dataset. it is just not a reliable
# report of most patient's true weight.

# examining likely predictor missingess and variation----

# below includes variables that are most likely to be included in the recipe,
# checking for missingness issues in these variables specifically
diabetic_NA_summary <- diabetic_clean |> 
  summarise(
    NA_race = sum(is.na(race)),
    NA_age = sum(is.na(age)),
    NA_gender = sum(is.na(gender)),
    NA_max_glu_serum = sum(is.na(max_glu_serum)),
    NA_a1cresult = sum(is.na(a1cresult)),
    NA_metformin = sum(is.na(metformin)),
    NA_change = sum(is.na(change)),
    NA_insulin = sum(is.na(insulin)),
    NA_num_lab_procedures = sum(is.na(num_lab_procedures)),
    NA_num_procedures = sum(is.na(num_procedures)),
    NA_num_medications = sum(is.na(num_medications)),
    NA_number_emergency = sum(is.na(number_emergency)),
    NA_time_in_hospital = sum(is.na(time_in_hospital)),
    NA_number_outpatient = sum(is.na(number_outpatient)),
    NA_number_inpatient = sum(is.na(number_inpatient)),
    NA_diabetes_med = sum(is.na(diabetes_med))
    # NA_ = sum(is.na()),
  )

diabetic_variance_summary <- diabetic_clean |> 
  summarise(
    sd_num_lab_procedures = sd(num_lab_procedures),
    sd_num_procedures = sd(num_procedures),
    sd_num_medications = sd(num_medications),
    sd_number_emergency = sd(number_emergency),
    sd_time_in_hospital = sd(time_in_hospital),
    sd_number_outpatient =sd(number_outpatient),
    sd_number_inpatient = sd(number_inpatient),
    # NA_ = sum(is.na()),
  )
# greatest variance in number of lab procedures

# examining likely predictor categorical breakdowns----

# race
race_demographics_plot <- diabetic_data |> 
  filter(race != "?") |> 
  ggplot(aes(x = race)) +
  geom_bar() +
  theme_minimal() +
  labs(
    title = "Racial Demographics of Diabetic Patients Admitted to Hospital"
  )

ggsave(here("results/race_demographics_plot.png"), race_demographics_plot)

# gender
gender_demographics_plot <- diabetic_data |> 
  filter(gender != "?") |> 
  ggplot(aes(x = gender)) +
  geom_bar() +
  theme_minimal() +
  labs(
    title = "Gender Demographics of Diabetic Patients Admitted to Hospital"
  )

ggsave(here("results/gender_demographics_plot.png"), gender_demographics_plot)

# age
age_demographics_plot <- diabetic_data |> 
  filter(age != "?") |> 
  ggplot(aes(x = age)) +
  geom_bar() +
  theme_minimal() +
  labs(
    title = "Age Demographics of Diabetic Patients Admitted to Hospital"
  )

ggsave(here("results/age_demographics_plot.png"), age_demographics_plot)

# target variable analysis----
diabetic_data |> skimr::skim_without_charts(readmitted)

readmitted_plot <- diabetic_data |> 
  ggplot(aes(x = readmitted)) +
  geom_bar() +
  theme_minimal() +
  labs(
    title = "Diabetic Patients: Readmission to Hospital Frequency"
  )

ggsave(here("memos/results/readmitted_plot.png"), readmitted_plot)
ggsave(here("results/readmitted_plot/png"), readmitted_plot)

readmitted_table <- diabetic_data |> 
  summarize(
    not_readmitted = sum(readmitted == "NO"),
    readmitted = sum(readmitted != "NO"),
    pct_not_readmitted = not_readmitted / (not_readmitted + readmitted) * 100,
    pct_readmitted = readmitted / (not_readmitted + readmitted) * 100,
  ) |> 
  select(pct_readmitted, pct_not_readmitted)

readmitted_table <- kable(readmitted_table, format = "html")

file_path_readmitted_table <- "memos/results/readmitted_table.html"

writeLines(as.character(readmitted_table), con = file_path_readmitted_table)




