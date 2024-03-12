# R Scripts
- This folder contains the R scripts used to clean/explore data, build/analyze models

### Scripts
- `0a_data_eda.R` contains the exploration of the dataset `diabetic_data.csv` and its variables relevant to our models.
- `0b_data_cleaning.R` contains the data collection and cleaning of the dataset `diabetic_data.csv`.
- `1_data_splitting.R` contains the splitting of the cleaned dataset into training and testing sets. It also includes the forming of a resampled folded dataset.
- `1_data_splitting.R` contains the splitting of the cleaned dataset into training and testing sets. It also includes the forming of a resampled folded dataset.
- `2_recipes.R` contains the formation of two different recipes, a simpler baseline model and a more complex tree-based model.
- `3a_log_reg.R` contains the fitting of the baseline logistic regression model.
- `3b_ridge.R` contains the fitting of the ridge model (applied to logistical approach).
- `3c_lasso.R` contains the fitting of the lasso model (applied to logistic approach).
- `3d_en.R` contains the tuning of the elastic net model
- `3e_knn.R` contains the tuning of the k-nearest neighbors model.
- `3f_boosted.R` contains the tuning of the gradient boosted tree model.
- `4_prelim_analysis.R` contains the preliminary analysis of the accuracy of fitted and tuned models
- `5_final_model_fit.R` fitting the final model to testing data
- `6_final_model_analysis.R` assessing the final model fit and accuracy