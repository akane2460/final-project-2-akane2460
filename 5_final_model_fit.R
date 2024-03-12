# Final project  ----
# Stat 301-1
# Final Model Fit

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

# load recipe
load(here("recipes/advanced_recipe.rda"))

# model specifications ----
ridge_spec <- 
  logistic_reg(penalty = 0, mixture = 1) |> 
  set_engine("glmnet", family = "binomial") |> 
  set_mode("classification") 

# advanced wflow----
advanced_ridge_wflow <-
  workflow() |> 
  add_model(ridge_spec) |> 
  add_recipe(advanced_recipe)

# advanced fit----
fit_ridge_final <- fit(advanced_ridge_wflow, diabetic_train)

# write out results----
save(fit_ridge_final, file = here("results/fit_ridge_final.rda"))
