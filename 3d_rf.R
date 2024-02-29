# Final project  ----
# Stat 301-1
# define and fit random forest

# random processes present

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(parallel)
library(doMC)

# handle common conflicts
tidymodels_prefer()

# parallel processing 
num_cores <- parallel::detectCores(logical = TRUE)
registerDoMC(cores = num_cores)

# load training data
load(here("data/diabetic_train.rda"))

summary(diabetic_train)

# load pre-processing/feature engineering/recipe
load(here("recipes/tree_based_diabetic_recipe.rda"))

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
  add_recipe(tree_based_diabetic_recipe)

# fit workflows/models
diabetic_fit_rf <- fit(diabetic_rf_wflow, diabetic_train)

      # diabetes_rf_fit <- diabetic_rf_wflow |>
      #   fit_resamples(
      #     resamples = diabetic_fold,
      #     control = control_resamples(save_workflow = TRUE)
      #     )

# save fit
save(diabetic_fit_rf, file = here("results/diabetic_fit_rf.rda"))




