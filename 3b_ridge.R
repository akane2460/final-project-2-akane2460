# Final project  ----
# Stat 301-1
# Define and fit of ridge (applied to logistical approach) model

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
load(here("data/diabetic_train.rda"))

# load resamples 
load(here("data/diabetic_fold.rda"))

# load pre-processing/feature engineering/recipe
load(here("recipes/null_diabetic_recipe.rda"))
load(here("recipes/featured_recipe.rda"))
load(here("recipes/advanced_recipe.rda"))

# model specifications ----
ridge_spec <- 
  logistic_reg(penalty = 0, mixture = 1) |> 
  set_engine("glmnet", family = "binomial") |> 
  set_mode("classification") 

# define workflows ----
# null wflow
null_ridge_wflow <-
  workflow() |> 
  add_model(ridge_spec) |> 
  add_recipe(null_diabetic_recipe)

# featured wflow
featured_ridge_wflow <-
  workflow() |> 
  add_model(ridge_spec) |> 
  add_recipe(featured_recipe)

# advanced wflow
advanced_ridge_wflow <-
  workflow() |> 
  add_model(ridge_spec) |> 
  add_recipe(advanced_recipe)

# fit workflows/models ----
# null fit
set.seed(02927328)

null_fit_ridge <- null_ridge_wflow |> 
  fit_resamples(
    resamples = diabetic_fold, 
    control = control_resamples(save_workflow = TRUE)
  )

# featured fit
set.seed(02840147)

featured_fit_ridge <- featured_ridge_wflow |> 
  fit_resamples(
    resamples = diabetic_fold, 
    control = control_resamples(save_workflow = TRUE)
  )

# advanced fit
set.seed(7843910)

advanced_fit_ridge <- advanced_ridge_wflow |> 
  fit_resamples(
    resamples = diabetic_fold, 
    control = control_resamples(save_workflow = TRUE)
  )

# fit_lasso <- fit(lasso_wflow, diabetic_train)

# write out results (fitted/trained workflows) ----
save(null_fit_ridge, file = here("results/null_fit_ridge.rda"))
save(featured_fit_ridge, file = here("results/featured_fit_ridge.rda"))
save(advanced_fit_ridge, file = here("results/advanced_fit_ridge.rda"))

