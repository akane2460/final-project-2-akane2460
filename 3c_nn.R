# L05 Resampling ----
# Define and fit nearest neighbor

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

set.seed(2201946)

# model specifications ----
# A nearest neighbor (`nearest_neighbor()`) with the `kknn` engine setting and `neighbors` set to 20.
knn_spec <- 
  nearest_neighbor(neighbors = 10) |> 
  set_engine("kknn") |> 
  set_mode("classification")

# knn_spec <- nearest_neighbor(mode = "classification", neighbors = 10) |> 
#   set_engine("kknn")

# define workflows ----
knn_wflow <-
  workflow() |> 
  add_model(knn_spec) |> 
  add_recipe(diabetic_recipe_lm)

# fit workflows/models ----
diabetic_fit_nn <- fit_resamples(knn_wflow, diabetic_fold)

# save fit
# save(diabetic_null_fit, file = here("results/diabetic_null_fit.rda"))