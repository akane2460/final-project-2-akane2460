# Final project  ----
# Stat 301-1
# Define and fit nearest neighbor

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
# load(here("data/diabetic_fold.rda"))
load(here("data/diabetic_train.rda"))

# load pre-processing/feature engineering/recipe
load(here("recipes/null_diabetic_recipe.rda"))

# set seed
set.seed(2201946)

# model specifications ----
# A nearest neighbor (`nearest_neighbor()`) with the `kknn` engine setting and `neighbors` set to 20.
knn_spec <- 
  nearest_neighbor(neighbors = 5) |> 
  set_engine("kknn") |> 
  set_mode("classification")

# define workflows ----
knn_wflow <-
  workflow() |> 
  add_model(knn_spec) |> 
  add_recipe(null_diabetic_recipe)

# fit workflows/models ----
diabetic_fit_nn <- fit(knn_wflow, diabetic_train)
# diabetic_fit_nn <- fit_resamples(knn_wflow, diabetic_fold)

# save fit
save(diabetic_fit_nn, file = here("results/diabetic_fit_nn.rda"))

