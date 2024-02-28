# Final project Data Splitting and Setup ----
# Stat 301-1

# random processes present

## Load Packages ----

library(tidyverse)
library(tidymodels)
library(skimr)
library(janitor)
library(here)
library(knitr)

tidymodels_prefer()

# read in data----
load(here("data/diabetic_clean.rda"))

# split----
# set seed
set.seed(239222)

# splitting data
diabetic_split <- diabetic_clean |> 
  initial_split(prop = .8, strata = readmitted)

# making train and test data
diabetic_train <- diabetic_split |> training()
diabetic_test <- diabetic_split |> testing()

# folding data (resamples)----
# set seed
set.seed(1245780)

# making fold data
diabetic_fold <- 
  diabetic_train |> 
    vfold_cv(v = 10, repeats = 5) 

# write out split, train, fold and test data
save(diabetic_split, file = here("data/diabetic_split.rda"))
save(diabetic_train, file = here("data/diabetic_train.rda"))
save(diabetic_test, file = here("data/diabetic_test.rda"))
save(diabetic_fold, file = here("data/diabetic_fold.rda"))
