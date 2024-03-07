# Final project  ----
# Stat 301-1
# define and fit boosted 

# random processes present

# note: fit not yet complete

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
load(here("recipes/tree_based_diabetic_recipe.rda"))

# model specifications ----
boosted_spec <- 
  boost_tree(mtry = tune(), min_n = tune(), learn_rate = tune()) |> 
  set_engine("xgboost") |> 
  set_mode("classification")

# define workflows ----
boosted_model <-
  workflow() |> 
  add_model(boosted_spec) |> 
  add_recipe(tree_based_diabetic_recipe)

# hyperparameter tuning values ----
# check ranges for hyperparameters
hardhat::extract_parameter_set_dials(boosted_model)

boosted_params <- parameters(boosted_model) |>  
  update(mtry = mtry(c(1, 14)), learn_rate = learn_rate(c(.075, .15)))

# Build a tuning grid
boosted_grid <- grid_regular(boosted_params, levels = 5)

# tuning model  ----
# set seed
set.seed(0927074)

boosted_tuned <- 
  boosted_model |> 
  tune_grid(
    carseats_fold, 
    grid = boosted_grid, 
    control = control_grid(save_workflow = TRUE)
  )

# write out results (tuned model) ----
save(boosted_tuned, file = here("results/boosted_tuned.rda"))


