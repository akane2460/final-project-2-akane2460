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

# load testing data
load(here("data/diabetic_test.rda"))

# load fits
load(here("results/fit_log_reg.rda"))
load(here("results/diabetic_fit_nn.rda"))
                              
# prediction tables
# log reg
predicted_log_reg <- bind_cols(diabetic_test, predict(fit_log_reg, diabetic_test)) |> 
  select(.pred_class, readmitted)

# accuracy
accuracy_log_reg <- accuracy(predicted_log_reg, truth = readmitted, estimate = .pred_class)
accuracy_log_reg_table <- accuracy_log_reg |> 
  knitr::kable()

# probabilities
readmit_probabilities <- bind_cols(diabetic_test, predict(fit_log_reg, diabetic_test, type = "prob")) |> 
  select(readmitted, .pred_YES, .pred_NO)

readmit_probabilities_table <- readmit_probabilities |> 
  knitr::kable()

# roc auc
roc_auc_curve_log_reg <- roc_auc(readmit_probabilities, truth = readmitted, .pred_YES)

roc_auc_curve_log_reg_table <- roc_auc_curve_log_reg |> 
  knitr::kable()

roc_auc_curve_log_reg_table





