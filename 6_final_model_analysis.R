# Final project  ----
# Stat 301-1
# Final Model Analysis

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

# load training data----
load(here("data/diabetic_test.rda"))

# load fit----
load(here("results/fit_ridge_final.rda"))

# predicted----
predicted_final <- bind_cols(diabetic_test, predict(fit_ridge_final, diabetic_test)) |> 
  select(.pred_class, readmitted)

# accuracy----
accuracy_final <- predicted_final |> 
  ungroup() |> 
  accuracy(truth = readmitted, estimate = .pred_class)

accuracy_final |>
  knitr::kable()

# probabilities----
class_probabilities <- bind_cols(diabetic_test, predict(fit_ridge_final, diabetic_test, type = "prob")) |> 
  select(.pred_YES, .pred_NO, readmitted)

# confusion matrix
confusion_matrix <- predicted_final |> 
  ungroup() |> 
  conf_mat(truth = readmitted, estimate = .pred_class)

as.data.frame.matrix(confusion_matrix$table) |> kable()

