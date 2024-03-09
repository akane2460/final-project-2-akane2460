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
load(here("data/diabetic_fold.rda"))

# load pre-processing/feature engineering/recipe
load(here("recipes/null_diabetic_recipe.rda"))
load(here("recipes/featured_recipe.rda"))
load(here("recipes/advanced_recipe.rda"))

# model specifications ----
log_reg_spec <- 
  logistic_reg() |> 
  set_engine("glm", maxit = 100000) |> 
  set_mode("classification") 

# define workflows ----
# null wflow
null_log_reg_wflow <-
  workflow() |> 
  add_model(log_reg_spec) |> 
  add_recipe(null_diabetic_recipe)

# featured wflow
featured_log_reg_wflow <-
  workflow() |> 
  add_model(log_reg_spec) |> 
  add_recipe(featured_recipe)

# advanced wflow
advanced_log_reg_wflow <-
  workflow() |> 
  add_model(log_reg_spec) |> 
  add_recipe(advanced_recipe)

# fit workflows/models ----
# fit resamples to null----
set.seed(986214)

null_fit_log_reg <- null_log_reg_wflow |> 
  fit_resamples(
  resamples = diabetic_fold, 
  control = control_resamples(save_workflow = TRUE)
)

# fit resamples to featured----
set.seed(024071924)

featured_fit_log_reg <- featured_log_reg_wflow |>
  fit_resamples(
    resamples = diabetic_fold,
    control = control_resamples(save_workflow = TRUE)
  )

# fit resamples to advanced----
set.seed(20193764)

advanced_fit_log_reg <- advanced_log_reg_wflow |>
  fit_resamples(
    resamples = diabetic_fold,
    control = control_resamples(save_workflow = TRUE)
  )


# fit to train data (if final model)
# fit_log_reg <-
#   fit(log_reg_wflow, diabetic_train)

# write out results (fitted/trained workflows) ----
save(null_fit_log_reg, file = here("results/null_fit_log_reg.rda"))
save(featured_fit_log_reg, file = here("results/featured_fit_log_reg.rda"))
save(advanced_fit_log_reg, file = here("results/advanced_fit_log_reg.rda"))

