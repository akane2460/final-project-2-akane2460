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
# load(here("data/diabetic_fold.rda"))
load(here("data/diabetic_train.rda"))

# load pre-processing/feature engineering/recipe
load(here("recipes/null_diabetic_recipe.rda"))