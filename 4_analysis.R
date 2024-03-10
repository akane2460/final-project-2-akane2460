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

# load testing data----
load(here("data/diabetic_test.rda"))

# load fits----
# log reg
load(here("results/null_fit_log_reg.rda"))
load(here("results/featured_fit_log_reg.rda"))
load(here("results/advanced_fit_log_reg.rda"))
# lasso
load(here("results/null_fit_lasso.rda"))
load(here("results/featured_fit_lasso.rda"))
load(here("results/advanced_fit_lasso.rda"))
# ridge
load(here("results/null_fit_ridge.rda"))
load(here("results/featured_fit_ridge.rda"))
load(here("results/advanced_fit_ridge.rda"))
# # boosted
load(here("results/null_boosted_tuned.rda"))
load(here("results/featured_boosted_tuned.rda"))
load(here("results/advanced_boosted_tuned.rda"))
# # en
# load(here("results/null_fit_log_reg.rda"))
# load(here("results/featured_fit_log_reg.rda"))
# load(here("results/advanced_fit_log_reg.rda"))
# knn
load(here("results/null_knn_tuned.rda"))
load(here("results/featured_knn_tuned.rda"))
load(here("results/advanced_knn_tuned.rda"))

# assessment of resamples----

# log reg assessment----
log_reg_model_set <- as_workflow_set(
  log_reg_null = null_fit_log_reg,
  log_reg_featured = featured_fit_log_reg,
  log_reg_advanced = advanced_fit_log_reg
)

log_reg_accuracy_metrics <- log_reg_model_set |>
  collect_metrics() |>
  filter(.metric == "accuracy")

log_reg_max_accuracy <- log_reg_accuracy_metrics |>
  group_by(wflow_id) |>
  slice_max(mean) |>
  distinct(wflow_id, .keep_all = TRUE)

log_reg_max_accuracy |>
  select(wflow_id, .metric, mean, std_err) |>
  knitr::kable()

  # performs best on advanced 

# ridge assesment----
ridge_model_set <- as_workflow_set(
  ridge_null = null_fit_log_reg,
  ridge_featured = featured_fit_ridge,
  ridge_advanced = advanced_fit_ridge)

ridge_accuracy_metrics <- ridge_model_set |>
  collect_metrics() |>
  filter(.metric == "accuracy")

ridge_max_accuracy <- ridge_accuracy_metrics |>
  group_by(wflow_id) |>
  slice_max(mean) |>
  distinct(wflow_id, .keep_all = TRUE)

ridge_max_accuracy |>
  select(wflow_id, .metric, mean, std_err) |>
  knitr::kable()

  # performs best on advanced

# lasso assessment----
lasso_model_set <- as_workflow_set(
  lasso_null = null_fit_lasso,
  lasso_featured = featured_fit_lasso,
  lasso_advanced = advanced_fit_lasso
)

lasso_accuracy_metrics <- lasso_model_set |>
  collect_metrics() |>
  filter(.metric == "accuracy")

lasso_max_accuracy <- lasso_accuracy_metrics |>
  group_by(wflow_id) |>
  slice_max(mean) |>
  distinct(wflow_id, .keep_all = TRUE)

lasso_max_accuracy |>
  select(wflow_id, .metric, mean, std_err) |>
  knitr::kable()

  # performs best on all 3

# boosted----
boosted_model_set <- as_workflow_set(
  boosted_null = null_boosted_tuned,
  boosted_featured = featured_boosted_tuned,
  boosted_advanced = advanced_boosted_tuned
)

boosted_accuracy_metrics <- boosted_model_set |>
  collect_metrics() |>
  filter(.metric == "accuracy")

boosted_max_accuracy <- boosted_accuracy_metrics |>
  group_by(wflow_id) |>
  slice_max(mean) |>
  distinct(wflow_id, .keep_all = TRUE)

boosted_max_accuracy |>
  select(wflow_id, .metric, mean, std_err) |>
  knitr::kable()

# performs best on null

# knn----
knn_model_set <- as_workflow_set(
  knn_null = null_knn_tuned,
  knn_featured = featured_knn_tuned,
  knn_advanced = advanced_knn_tuned
)

knn_accuracy_metrics <- knn_model_set |>
  collect_metrics() |>
  filter(.metric == "accuracy")

knn_max_accuracy <- knn_accuracy_metrics |>
  group_by(wflow_id) |>
  slice_max(mean) |>
  distinct(wflow_id, .keep_all = TRUE)

knn_max_accuracy |>
  select(wflow_id, .metric, mean, std_err) |>
  knitr::kable()

# performs best on advanced

# overall assessment----
model_set <- as_workflow_set(
  log_reg_null = null_fit_log_reg,
  ridge_null = null_fit_log_reg,
  lasso_null = null_fit_lasso,
  # boosted_null = null_fit_log_reg,
  # en_null = null_fit_log_reg,
  # knn_null = null_fit_log_reg,
  log_reg_featured = featured_fit_log_reg,
  ridge_featured = featured_fit_ridge,
  lasso_featured = featured_fit_lasso,
  # boosted_null = null_fit_log_reg,
  # en_null = null_fit_log_reg,
  # knn_null = null_fit_log_reg,
  log_reg_advanced = advanced_fit_log_reg,
  ridge_advanced = advanced_fit_ridge,
  lasso_advanced = advanced_fit_lasso
  # boosted_null = null_fit_log_reg,
  # en_null = null_fit_log_reg,
  # knn_null = null_fit_log_reg,
)

accuracy_metrics <- model_set |>
  collect_metrics() |>
  filter(.metric == "accuracy")

max_accuracy <- accuracy_metrics |>
  group_by(wflow_id) |>
  slice_max(mean) |>
  distinct(wflow_id, .keep_all = TRUE)

max_accuracy |>
  select(wflow_id, .metric, mean, std_err) |>
  knitr::kable()

# # look at best results parameters rf
# best_results_rf <- select_best(rf_tuned, metric = "accuracy")


