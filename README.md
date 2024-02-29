## Diabetes Hospitalization: Risk for Readmission
This repo includes analysis on the dataset `diabetic_data.csv`, with data obtained in from years 1999 to 2008. on paitents with diabetes and their stays in the hospital.

## What's in the Repo

### Folders
- `data\` can find the original dataset `diabetic_data.csv` and the id codebook `IDS_mapping.csv`.
- `memos\` can find the data memos for the project
- `recipes\` can find the recipes for the project
- `results\` can find outputs to be included in memos or reports in the project

### R Scripts
- `0a_data_eda.R` contains the exploration of the dataset `diabetic_data.csv` and its variables relevant to our models.
- `0b_data_cleaning.R` contains the data collection and cleaning of the dataset `diabetic_data.csv`.
- `1_data_splitting.R` contains the splitting of the cleaned dataset into training and testing sets. It also includes the forming of a resampled folded dataset.
- `1_data_splitting.R` contains the splitting of the cleaned dataset into training and testing sets. It also includes the forming of a resampled folded dataset.
- `2_recipes.R` contains the formation of two different recipes, a simpler baseline model and a more complex tree-based model.
- `3a_log_reg.R` contains the fitting of the baseline logistical regression model.
- `3b_knn.R` contains the fitting of the k-nearest neighbors model.
- `3c_lasso.R` contains the fitting of the lasso model (applied to logistical approach).
- `3d_ridge.R` contains the fitting of the ridge model (applied to logistical approach).
- `3e_rf.R` contains the fitting of the random forest model.
- `3f_boosted.R` contains the fitting of the gradient boosted tree model.

### Quarto Documents
- Within the memos folder:
    - `Kane_Allison_progress_memo_1.qmd` contains the first data memo and project progress on the dataset.
    - `Kane_Allison_progress_memo_2.qmd` contains the second data memo and project progress on the dataset.
- Within main folder:
    - `Kane_Allison_final_report.qmd` contains the final report on the project.
    - `Kane_Allison_executive_summary.qmd` contains the executive summary on the project.
    
### HTML Documents
- Within the memos folder:
    - `Kane_Allison_progress_memo_1.html` contains the first data memo and project progress on the dataset.
    - `Kane_Allison_progress_memo_2.html` contains the second data memo and project progress on the dataset.
- Within main folder:
    - `Kane_Allison_final_report.html` contains the final report on the project.
    - `Kane_Allison_executive_summary.html` contains the executive summary on the project.
