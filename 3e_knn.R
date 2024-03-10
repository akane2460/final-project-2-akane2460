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

# load pre-processing/feature engineering/recipe
load(here("recipes/null_diabetic_recipe.rda"))
load(here("recipes/featured_recipe.rda"))
load(here("recipes/advanced_recipe.rda"))

# set seed
set.seed(907012463)

# model specifications----
kknn_spec <- 
  nearest_neighbor(neighbors = tune()) |> 
  set_engine("kknn") |> 
  set_mode("classification")

# define workflows ----
null_knn_wflow <-
  workflow() |> 
  add_model(kknn_spec) |> 
  add_recipe(null_diabetic_recipe)

# featured wflow
featured_knn_wflow <-
  workflow() |> 
  add_model(kknn_spec) |> 
  add_recipe(featured_recipe)

# advanced wflow
advanced_knn_wflow <-
  workflow() |> 
  add_model(kknn_spec) |> 
  add_recipe(advanced_recipe)

# hyperparameter tuning values ----
# null hyperparameters
hardhat::extract_parameter_set_dials(null_knn_wflow)

null_knn_params <- parameters(null_knn_wflow) |>  
  update(neighbors = neighbors())

null_knn_grid <- grid_regular(null_knn_params, levels = 8)

# featured hyperparameters
hardhat::extract_parameter_set_dials(featured_knn_wflow)

featured_knn_params <- parameters(featured_knn_wflow) |>  
  update(neighbors = neighbors())

featured_knn_grid <- grid_regular(featured_knn_params, levels = 8)

# advanced hyperparameters
hardhat::extract_parameter_set_dials(advanced_knn_wflow)

advanced_knn_params <- parameters(advanced_knn_wflow) |>  
  update(neighbors = neighbors())

advanced_knn_grid <- grid_regular(advanced_knn_params, levels = 8)

# fit workflows/models ----
# null
# set seed
set.seed(02917023)
null_knn_tuned <- 
  null_knn_wflow |> 
  tune_grid(
    diabetic_fold, 
    grid = null_knn_grid, 
    control = control_grid(save_workflow = TRUE)
  )

# featured
# set seed
set.seed(021947965)
featured_knn_tuned <- 
  featured_knn_wflow |> 
  tune_grid(
    diabetic_fold, 
    grid = featured_knn_grid, 
    control = control_grid(save_workflow = TRUE)
  )


# advanced
set.seed(64912780)
advanced_knn_tuned <- 
  advanced_knn_wflow |> 
  tune_grid(
    diabetic_fold, 
    grid = advanced_knn_grid, 
    control = control_grid(save_workflow = TRUE)
  )


# save fit
save(null_knn_tuned, file = here("results/null_knn_tuned.rda"))
save(featured_knn_tuned, file = here("results/featured_knn_tuned.rda"))
save(advanced_knn_tuned, file = here("results/advanced_knn_tuned.rda"))

