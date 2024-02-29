# Final project  ----
# Stat 301-1
# define and fit random forest

# note: not fitted yet

# random processes present

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)

# handle common conflicts
tidymodels_prefer()

# load training data
load(here("data/diabetic_train.rda"))
load(here("data/diabetic_fold.rda"))

# load pre-processing/feature engineering/recipe
load(here("recipes/null_diabetic_recipe.rda"))

# set seed
set.seed(9862349)

random_forest_spec <-
  rand_forest() |>
  set_engine("ranger") |>
  set_mode("classification")

# define workflow
diabetic_rf_wflow <-
  workflow() |>
  add_model(random_forest_spec) |>
  add_recipe(diabetic_recipe_lm)

# fit workflows/models
# diabetes_rf_fit <- diabetic_rf_wflow |> 
#   fit_resamples(
#     resamples = diabetic_fold,
#     control = control_resamples(save_workflow = TRUE)
#     )






