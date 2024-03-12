# Final project Data Cleaning ----
# Stat 301-1

# random processes present

## Load Packages ----

library(tidyverse)
library(skimr)
library(janitor)
library(here)
library(knitr)

# read in data
diabetic_data <- read_csv(here("data/diabetic_data.csv"))

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
# checking for issues to be cleaned in 0b_data_cleaning

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
diabetic_NA_summary <- diabetic_data |> 
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

diabetic_variance_summary <- diabetic_data |> 
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

# examining "Unknown/Invalid" for gender----
diabetic_data |> 
  filter(gender == "Unknown/Invalid") |> 
  nrow()
# there are only 3 instances 
# not large enough sample to make any substantial conclusions
# will exclude from further analysis
diabetic_data <- diabetic_data |> 
  filter(gender != "Unknown/Invalid")

## cleaning----
# clean names----
diabetic_clean <- diabetic_data |> janitor::clean_names()

# removing weight from dataset----
diabetic_clean <- diabetic_clean |> 
  select(-weight)

# removing medical_specialty from dataset----
#  missigness issues nearly 50% of the time
# not a reliable reporter of admitting physicians specialty
diabetic_clean <- diabetic_clean |> 
  select(-medical_specialty)

# removing payer_code from dataset----
# missigness issues 40% of the time
# not a reliable reporter of patient's insurance status
# missing value does not indicate no insurance (self-pay does)
diabetic_clean <- diabetic_clean |> 
  select(-payer_code)

# removing "?"s and recplaing with NAs----
diabetic_clean <- diabetic_clean |> 
  mutate(
    race = ifelse(race == "?", NA, race),
    diag_1 = ifelse(diag_1 == "?", NA, diag_1),
    diag_2 = ifelse(diag_2 == "?", NA, diag_2),
    diag_3 = ifelse(diag_3 == "?", NA, diag_3)
  )

# checking transformations thus far----
diabetic_clean |> skimr::skim_without_charts()
  # checking to ensure almost no missingness issues

# processing demographic information----
diabetic_clean <- diabetic_clean |> 
  mutate(
    race = factor(race),
    age = factor(age, levels = c("[0-10)", "[10-20)", "[20-30)", "[30-40)",
                                 "[40-50)", "[50-60)", "[60-70)", "[70-80)",
                                 "[80-90)", "[90-100)"), ordered = TRUE),
    gender = factor(gender))

# processing medical information (not drugs)
diabetic_clean <- diabetic_clean |> 
  mutate(
    max_glu_serum = factor(max_glu_serum, levels = c("None", "Normal", ">200", ">300"), ordered = TRUE),
    a1cresult = factor(a1cresult, levels = c("None", "Norm", ">7", ">8"), ordered = TRUE),
    change = factor(change, levels = c("No", "Ch")),
    diabetes_med = factor(diabetes_med)
    )

# factoring all the possible drugs they could take
  # these might be adjusted based on what is appropriate for the model
diabetic_clean <- diabetic_clean |> 
  mutate(
    metformin = factor(metformin, levels = c("No", "Down", "Steady", "Up"), ordered = TRUE),
    repaglinide = factor(repaglinide, levels = c("No", "Down", "Steady", "Up"), ordered = TRUE),
    nateglinide = factor(nateglinide, levels = c("No", "Down", "Steady", "Up"), ordered = TRUE),
    chlorpropamide = factor(chlorpropamide, levels = c("No", "Down", "Steady", "Up"), ordered = TRUE),
    glimepiride = factor(glimepiride, levels = c("No", "Down", "Steady", "Up"), ordered = TRUE),
    acetohexamide = factor(acetohexamide),
    glipizide = factor(glipizide, levels = c("No", "Down", "Steady", "Up"), ordered = TRUE),
    glyburide = factor(glyburide, levels = c("No", "Down", "Steady", "Up"), ordered = TRUE),
    tolbutamide = factor(tolbutamide, levels = c("No", "Steady"), ordered = TRUE),
    pioglitazone = factor(pioglitazone, levels = c("No", "Down", "Steady", "Up"), ordered = TRUE),
    rosiglitazone = factor(rosiglitazone, levels = c("No", "Down", "Steady", "Up"), ordered = TRUE),
    acarbose = factor(acarbose, levels = c("No", "Down", "Steady", "Up"), ordered = TRUE),
    miglitol = factor(miglitol, levels = c("No", "Down", "Steady", "Up"), ordered = TRUE),
    troglitazone = factor(troglitazone, levels = c("No", "Steady"), ordered = TRUE),
    tolazamide = factor(tolazamide, levels = c("No", "Steady", "Up"), ordered = TRUE),
    examide = factor(examide),
    citoglipton = factor(citoglipton),
    insulin = factor(insulin, levels = c("No", "Down", "Steady", "Up"), ordered = TRUE),
    glyburide_metformin = factor(glyburide_metformin, levels = c("No", "Down", "Steady", "Up"), ordered = TRUE),
    glipizide_metformin = factor(glipizide_metformin),
    glimepiride_pioglitazone = factor(glimepiride_pioglitazone),
    metformin_rosiglitazone = factor(metformin_rosiglitazone),
    metformin_pioglitazone = factor(metformin_pioglitazone)
  )

# removing NAs race and max_glu_serum----
# removing observations with missingness in variables that are particularly 
# important to our analysis (race and max_glu_serum are very important)
diabetic_clean <- diabetic_clean |> 
  filter(is.na(race) == FALSE) 

diabetic_clean <- diabetic_clean |> 
  filter(is.na(max_glu_serum) == FALSE)

# adjusting target variable----
  # readmitted has 3 categories
    # in our case we only would like 2 (whether they returned to the hospital)

# diabetic_clean$readmitted

diabetic_clean <- diabetic_clean |> 
  mutate(
    readmitted = ifelse(readmitted != "NO", "YES", "NO")
  )

diabetic_clean <- diabetic_clean |> 
  mutate(
    readmitted = factor(readmitted)
  )

# checking cleaned data----
skim_without_charts(diabetic_clean)

# save out cleaned data----
save(diabetic_clean, file = here("data/diabetic_clean.rda"))

skim_without_charts(diabetic_clean)

# sampling cleaned data----
set.seed(2109480)
sampled_diabetic <- diabetic_clean %>% 
  group_by(readmitted) %>% 
  sample_n(size = 18000 / n_distinct(readmitted))

# save out cleaned sample data----
save(sampled_diabetic, file = here("data/sampled_diabetic.rda"))
