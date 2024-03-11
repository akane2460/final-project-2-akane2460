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
  logistic_reg(penalty = 1, mixture = .5) |> 
  set_engine("glmnet", family = "binomial") |> 
  set_mode("classification") 

# define workflows ----
# null wflow
null_en_wflow <-
  workflow() |> 
  add_model(elastic_net_spec) |> 
  add_recipe(null_diabetic_recipe)

# featured wflow
featured_en_wflow <-
  workflow() |> 
  add_model(elastic_net_spec) |> 
  add_recipe(featured_recipe)

# advanced wflow
advanced_en_wflow <-
  workflow() |> 
  add_model(elastic_net_spec) |> 
  add_recipe(advanced_recipe)

# fit workflows/models ----
# null fit
set.seed(01927340)

null_fit_en <- null_en_wflow |> 
  fit_resamples(
    resamples = diabetic_fold, 
    control = control_resamples(save_workflow = TRUE)
  )

# featured fit
set.seed(01283464)

featured_fit_en <- featured_en_wflow |> 
  fit_resamples(
    resamples = diabetic_fold, 
    control = control_resamples(save_workflow = TRUE)
  )

# advanced fit
set.seed(99109397)

advanced_fit_en <- advanced_en_wflow |> 
  fit_resamples(
    resamples = diabetic_fold, 
    control = control_resamples(save_workflow = TRUE)
  )

# fit_lasso <- fit(lasso_wflow, diabetic_train)

# write out results (fitted/trained workflows) ----
save(null_fit_en, file = here("results/null_fit_en.rda"))
save(featured_fit_en, file = here("results/featured_fit_en.rda"))
save(advanced_fit_en, file = here("results/advanced_fit_en.rda"))
