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

# specification
diabetic_null_spec <- null_model() %>% 
  set_engine("parsnip") %>% 
  set_mode("classification")

# define wflow
diabetic_null_workflow <- workflow() %>% 
  add_model(diabetic_null_spec) %>% 
  add_recipe(diabetic_recipe_lm)

# fit model
diabetic_null_fit <- diabetic_null_workflow |> 
  fit_resamples(
    resamples = diabetic_fold, 
    control = control_resamples(save_workflow = TRUE)
  )

# save fit
save(diabetic_null_fit, file = here("results/diabetic_null_fit.rda"))
