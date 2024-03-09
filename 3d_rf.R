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
load(here("data/diabetic_fold.rda"))

# load pre-processing/feature engineering/recipe
load(here("recipes/null_diabetic_recipe.rda"))

# model specifications ----
# set seed
set.seed(2109739)

rf_spec <- 
  rand_forest(trees = 500, min_n = tune(), mtry = tune()) |> 
  set_engine("ranger") |> 
  set_mode("classification")

# define workflows ----
rf_model <-
  workflow() |> 
  add_model(rf_spec) |> 
  add_recipe(null_diabetic_recipe)

# hyperparameter tuning values ----

# change hyperparameter ranges
rf_params <- hardhat::extract_parameter_set_dials(rf_model) %>% 
  update(mtry = mtry(c(1, 14))) 

# build tuning grid
rf_grid <- grid_regular(rf_params, levels = 5)

# fit workflows/models ----
# set seed
set.seed(0127307)
rf_tuned <- 
  rf_model |> 
  tune_grid(
    diabetic_fold, 
    grid = rf_grid, 
    control = control_grid(save_workflow = TRUE)
  )

# save fit
save(diabetic_fit_rf, file = here("results/diabetic_fit_rf.rda"))


