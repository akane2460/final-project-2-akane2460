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

# load resamples 
load(here("data/diabetic_fold.rda"))

# load pre-processing/feature engineering/recipe
load(here("recipes/null_diabetic_recipe.rda"))
load(here("recipes/featured_recipe.rda"))
load(here("recipes/advanced_recipe.rda"))

# model specifications ----
elastic_net_spec <- 
  logistic_reg(penalty = tune(), mixture = tune()) |> 
  set_engine("glmnet", family = "binomial") |> 
  set_mode("classification") 

# define workflows ----
# null wflow
null_en_wflow <-
  workflow() |> 
  add_model(elastic_net_spec) |> 
  add_recipe(null_diabetic_recipe)

featured_en_wflow <-
  workflow() |> 
  add_model(elastic_net_spec) |> 
  add_recipe(featured_recipe)

advanced_en_wflow <-
  workflow() |> 
  add_model(elastic_net_spec) |> 
  add_recipe(advanced_recipe)

# hyperparameter tuning values ----
# null hyperparameters
hardhat::extract_parameter_set_dials(null_en_wflow)

null_en_params <- parameters(null_en_wflow) |>  
  update(penalty = penalty(c(0, 1)), mixture = mixture(c(0, 1)))

null_en_grid <- grid_regular(null_en_params, levels = 5)

# featured hyperparameters
hardhat::extract_parameter_set_dials(featured_en_wflow)

featured_en_params <- parameters(featured_en_wflow) |>  
  update(penalty = penalty(c(0, 1)), mixture = mixture(c(0, 1)))

featured_en_grid <- grid_regular(null_en_params, levels = 5)

# advanced hyperparameters
hardhat::extract_parameter_set_dials(advanced_en_wflow)

advanced_en_params <- parameters(advanced_en_wflow) |>  
  update(penalty = penalty(c(0, 1)), mixture = mixture(c(0, 1)))

advanced_en_grid <- grid_regular(null_en_params, levels = 5)

# tune workflows/models ----
# null tune
set.seed(109274)
null_en_tuned <- 
  null_en_wflow |> 
  tune_grid(
    diabetic_fold, 
    grid = null_en_grid, 
    control = control_grid(save_workflow = TRUE)
  )

# featured tune
set.seed(109274)
featured_en_tuned <- 
  featured_en_wflow |> 
  tune_grid(
    diabetic_fold, 
    grid = featured_en_grid, 
    control = control_grid(save_workflow = TRUE)
  )

# advanced tune
set.seed(109274)
advanced_en_tuned <- 
  advanced_en_wflow |> 
  tune_grid(
    diabetic_fold, 
    grid = advanced_en_grid, 
    control = control_grid(save_workflow = TRUE)
  )

# write out results (fitted/trained workflows) ----
save(null_en_tuned, file = here("results/null_en_tuned.rda"))
save(featured_en_tuned, file = here("results/featured_en_tuned.rda"))
save(advanced_en_tuned, file = here("results/advanced_en_tuned.rda"))
