# Final project  ----
# Stat 301-1
# Define and fit nearest neighbor

# note: not fitted yet

# random processes present

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(doMC)
library(parallel)

# handle common conflicts
tidymodels_prefer()

# parallel processing 
num_cores <- parallel::detectCores(logical = TRUE)

registerDoMC(cores = num_cores)

# load training data
load(here("data/diabetic_fold.rda"))
load(here("data/diabetic_train.rda"))

# load pre-processing/feature engineering/recipe
load(here("recipes/null_diabetic_recipe.rda"))

# set seed
set.seed(907012463)

# model specifications----
kknn_spec <- 
  nearest_neighbor(neighbors = tune()) |> 
  set_engine("kknn") |> 
  set_mode("classification")

# define workflows ----
kknn_model <-
  workflow() |> 
  add_model(kknn_spec) |> 
  add_recipe(null_diabetic_recipe)

# hyperparameter tuning values ----

# check ranges for hyperparameters
hardhat::extract_parameter_set_dials(kknn_model)

# change hyperparameter ranges
kknn_params <- parameters(kknn_model)

# build tuning grid
kknn_grid <- grid_regular(kknn_params, levels = 5)

# fit workflows/models ----
# set seed
set.seed(29874223)

kknn_tuned <- 
  kknn_model |> 
  tune_grid(
    diabetic_fold, 
    grid = kknn_grid, 
    control = control_grid(save_workflow = TRUE)
  )

# save fit
save(diabetic_fit_nn, file = here("results/diabetic_fit_nn.rda"))

