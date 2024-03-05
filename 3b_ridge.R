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
load(here("recipes/lasso_recipe.rda"))

# model specifications ----
ridge_spec <- 
  logistic_reg(penalty = 0, mixture = 1) |> 
  set_engine("glmnet", family = "binomial") |> 
  set_mode("classification") 

# define workflows ----
ridge_wflow <-
  workflow() |> 
  add_model(ridge_spec) |> 
  add_recipe(lasso_recipe)

# fit workflows/models ----
set.seed(02927328)

fit_ridge <- ridge_wflow |> 
  fit_resamples(
    resamples = diabetic_fold, 
    control = control_resamples(save_workflow = TRUE)
  )

# fit_lasso <- fit(lasso_wflow, diabetic_train)

# write out results (fitted/trained workflows) ----
save(fit_ridge, file = here("results/fit_ridge.rda"))
