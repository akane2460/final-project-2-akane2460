# Final project Data Cleaning ----
# Stat 301-1
## Load Packages ----

library(tidyverse)
library(skimr)
library(janitor)
library(here)
library(knitr)

# read in data
diabetic_data <- read_csv(here("data/diabetic_data.csv"))

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
    # in our case we only would like 2 (whether they returned to the hospital or not)
      # for now

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
