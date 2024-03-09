
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
load(here("recipes/null_diabetic_recipe.rda"))
load(here("recipes/featured_recipe.rda"))
load(here("recipes/advanced_recipe.rda"))

# model specifications ----
boosted_spec <- 
  boost_tree(mtry = tune(), min_n = tune(), learn_rate = tune()) |> 
  set_engine("xgboost") |> 
  set_mode("classification")

# define workflows ----
# null wflow
null_boosted_wflow <-
  workflow() |> 
  add_model(boosted_spec) |> 
  add_recipe(null_diabetic_recipe)

# featured wflow
featured_boosted_wflow <-
  workflow() |> 
  add_model(boosted_spec) |> 
  add_recipe(featured_recipe)

# advanced wflow
advanced_boosted_wflow <-
  workflow() |> 
  add_model(boosted_spec) |> 
  add_recipe(advanced_recipe)

# hyperparameter tuning values ----
# null hyperparameters
hardhat::extract_parameter_set_dials(null_boosted_wflow)

null_boosted_params <- parameters(null_boosted_wflow) |>  
  update(mtry = mtry(c(1, 14)), learn_rate = learn_rate(c(.075, .15)))

null_boosted_grid <- grid_regular(null_boosted_params, levels = 5)

# featured hyperparameters
hardhat::extract_parameter_set_dials(featured_boosted_wflow)

featured_boosted_params <- parameters(featured_boosted_wflow) |>  
  update(mtry = mtry(c(1, 14)), learn_rate = learn_rate(c(.075, .15)))

featured_boosted_grid <- grid_regular(featured_boosted_params, levels = 5)

# advanced hyperparameters
hardhat::extract_parameter_set_dials(advanced_boosted_wflow)

advanced_boosted_params <- parameters(advanced_boosted_wflow) |>  
  update(mtry = mtry(c(1, 14)), learn_rate = learn_rate(c(.075, .15)))

advanced_boosted_grid <- grid_regular(advanced_boosted_params, levels = 5)

# tuning model----
# null
# set seed
set.seed(0927074)
null_boosted_tuned <- 
  null_boosted_wflow |> 
  tune_grid(
    diabetic_fold, 
    grid = null_boosted_grid, 
    control = control_grid(save_workflow = TRUE)
  )

# featured
# set seed
set.seed(07841234)
featured_boosted_tuned <- 
  featured_boosted_wflow |> 
  tune_grid(
    diabetic_fold, 
    grid = featured_boosted_grid, 
    control = control_grid(save_workflow = TRUE)
  )

# advanced
set.seed(01894723)
advanced_boosted_tuned <- 
  advanced_boosted_wflow|> 
  tune_grid(
    diabetic_fold, 
    grid = advanced_boosted_grid, 
    control = control_grid(save_workflow = TRUE)
  )

# write out results (tuned model) ----
save(null_boosted_tuned, file = here("results/null_boosted_tuned.rda"))
save(featured_boosted_tuned, file = here("results/featured_boosted_tuned.rda"))
save(advanced_boosted_tuned, file = here("results/advanced_boosted_tuned.rda"))


