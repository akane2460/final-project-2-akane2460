# Final project Recipes ----
# Stat 301-1

# model recipes

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
load(here("data/diabetic_test.rda"))

# recipe a: simple recipe----

# description of what is in the recipe
    # for initial recipe, the general category change will be examined as a placeholder
      # for the more specific medication variables like metformin or insulin (etc)
      # the change variable encompasses if there is any change in ANY diabetes medication
      # whereas these specific medication variables measure if that medication
      # changed in its prescription. 
      # in the future, accounting for the different effects of different drugs
      # might be worthwhile. but for now we are examining overall changes in diabetes medication
    
    # basic demographics (age, race, gender) and their lab results (max_glu_serum and a1cresult)
    # are important assessments of their risk for certain complications of diabetes
    
    # the number of procedures, doctor visits, days in the hospital, etc. are all indicators
    # of how severe their side effects or complications of diabetes could be

# recipe
null_diabetic_recipe <- recipe(
  readmitted ~ age + race + gender + max_glu_serum + a1cresult
  + change + num_lab_procedures + num_procedures + num_medications + number_diagnoses +
    number_emergency + time_in_hospital + number_outpatient + number_inpatient,
  data = diabetic_train) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE)
      # additional interactions might be considered in future

# testing recipe
# null_diabetic_recipe |>
#   prep() |>
#   bake(new_data = NULL) |>
#   glimpse()

# save recipe
save(null_diabetic_recipe, file = here("recipes/null_diabetic_recipe.rda"))

# recipe b: lasso----
lasso_recipe <- recipe(
  readmitted ~ age + race + gender + max_glu_serum + a1cresult
  + change + num_lab_procedures + num_procedures + num_medications + number_diagnoses +
    number_emergency + time_in_hospital + number_outpatient + number_inpatient,
  data = diabetic_train) |> 
  step_nzv(all_predictors()) |>  
  step_center(all_numeric_predictors()) |>  
  step_scale(all_numeric_predictors()) |> 
  step_dummy(all_nominal_predictors(), one_hot = FALSE) 

# save recipe
save(lasso_recipe, file = here("recipes/lasso_recipe.rda"))

# recipe c: tree based----
tree_based_diabetic_recipe <- recipe(
  readmitted ~ age + race + gender + max_glu_serum + a1cresult
  + change + num_lab_procedures + num_procedures + num_medications + number_diagnoses +
    number_emergency + time_in_hospital + number_outpatient + number_inpatient,
  data = diabetic_train) |> 
  step_interact(~ time_in_hospital : c(num_lab_procedures, number_diagnoses, num_procedures)) |> 
  # with more time in the hospital, typically doing more tests/procedures and 
  # providing more diagnoses
  step_interact(~ number_emergency : c(number_outpatient, number_inpatient))
# if you have a medical emergency, typically you will receive either outpatient or inpatient
# medical care
  

save(tree_based_diabetic_recipe, file = here("recipes/tree_based_diabetic_recipe.rda"))
