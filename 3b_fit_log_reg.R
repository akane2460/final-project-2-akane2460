# L05 Resampling ----
# Define and fit ridge

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)

# handle common conflicts
tidymodels_prefer()

# load training data
load(here("data/diabetic_fold.rda"))

# load pre-processing/feature engineering/recipe
load(here("recipes/diabetic_recipe_lm.rda"))

# model specifications ----
log_reg_spec <- 
  logistic_reg() |> 
  set_engine("glm") |> 
  set_mode("classification") 

# define workflows ----
log_reg_wflow <-
  workflow() |> 
  add_model(log_reg_spec) |> 
  add_recipe(diabetic_recipe_lm)

# fit workflows/models ----
fit_log_reg <- fit(log_reg_wflow, diabetic_fold)

# save fit
# save(diabetic_null_fit, file = here("results/diabetic_null_fit.rda"))