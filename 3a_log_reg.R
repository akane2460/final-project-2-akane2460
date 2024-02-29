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
load(here("recipes/diabetic_recipe_lm.rda"))

# model specifications ----
log_reg_spec <- 
  logistic_reg() |> 
  set_engine("glm", maxit = 100000) |> 
  set_mode("classification") 

# define workflows ----
log_reg_wflow <-
  workflow() |> 
  add_model(log_reg_spec) |> 
  add_recipe(diabetic_recipe_lm)

# fit workflows/models ----
fit_log_reg <- fit(log_reg_wflow, diabetic_train)

# fit_log_reg_folded <- log_reg_wflow |> 
#   fit_resamples(resamples = diabetic_fold,
#                 control = control_resamples(save_workflow = TRUE))

# write out results (fitted/trained workflows) ----
save(fit_log_reg, file = here("results/fit_log_reg.rda"))
# save(fit_log_reg_folded, file = here("results/fit_log_reg_folded.rda"))



