# Final project  ----
# Stat 301-1
# Define and fit lasso 
# note: not fitted to folds yet

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(foreach)
library(doMC)
library(parallel)

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
lasso_spec <- 
  logistic_reg(penalty = 1, mixture = 1) |> 
  set_engine("glmnet", family = "binomial") |> 
  set_mode("classification") 

# define workflows ----
# null wflow
null_lasso_wflow <-
  workflow() |> 
  add_model(lasso_spec) |> 
  add_recipe(null_diabetic_recipe)

# featured wflow
featured_lasso_wflow <-
  workflow() |> 
  add_model(lasso_spec) |> 
  add_recipe(featured_recipe)

# advanced wflow
advanced_lasso_wflow <-
  workflow() |> 
  add_model(lasso_spec) |> 
  add_recipe(advanced_recipe)

# fit workflows/models ----
# fit resamples to null----
set.seed(13749208)

null_fit_lasso <- null_lasso_wflow |> 
  fit_resamples(
    resamples = diabetic_fold, 
    control = control_resamples(save_workflow = TRUE)
  )

# fit resamples to featured----
set.seed(02753891)

featured_fit_lasso <- featured_lasso_wflow |>
  fit_resamples(
    resamples = diabetic_fold,
    control = control_resamples(save_workflow = TRUE)
  )

# fit resamples to advanced----
set.seed(014798311)

advanced_fit_lasso <- advanced_lasso_wflow |>
  fit_resamples(
    resamples = diabetic_fold,
    control = control_resamples(save_workflow = TRUE)
  )

# fit_lasso <- fit(lasso_wflow, diabetic_train)

# write out results (fitted/trained workflows) ----
save(null_fit_lasso, file = here("results/null_fit_lasso.rda"))
save(featured_fit_lasso, file = here("results/featured_fit_lasso.rda"))
save(advanced_fit_lasso, file = here("results/advanced_fit_lasso.rda"))

