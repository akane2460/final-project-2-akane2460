# Final project Recipes ----
# Stat 301-1

# Logistic Model recipe

### Load Packages ----

library(tidyverse)
library(tidymodels)
library(skimr)
library(janitor)
library(here)
library(knitr)

tidymodels_prefer()

# load data
load(here("data/diabetic_train.rda"))

# recipe b: logistic model----

# recipe
logitistic_diabetic_recipe <- recipe(
  readmitted ~ age + race + gender + max_glu_serum + a1cresult
  + change + num_lab_procedures + num_procedures + num_medications + number_diagnoses +
    number_emergency + time_in_hospital + number_outpatient + number_inpatient,
  data = diabetic_train) |>
  step_dummy(all_nominal_predictors())

# testing recipe
logitistic_diabetic_recipe |>
  prep() |>
  bake(new_data = NULL) |>
  glimpse()



