# Final project  ----
# Stat 301-1
# Define and fit logistic regression

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

# load pre-processing/feature engineering/recipe
load(here("recipes/null_diabetic_recipe.rda"))

# model specifications ----
lasso_spec <- 
  logistic_reg(penalty = 1, mixture = 1) |> 
  set_engine("glmnet", family = "binomial") |> 
  set_mode("classification") 

# define workflows ----
lasso_wflow <-
  workflow() |> 
  add_model(lasso_spec) |> 
  add_recipe(null_diabetic_recipe)

# fit workflows/models ----
fit_lasso <- fit(lasso_wflow, diabetic_train)

# write out results (fitted/trained workflows) ----
save(fit_lasso, file = here("results/fit_lasso.rda"))

fit_lasso |> 
head(4)

