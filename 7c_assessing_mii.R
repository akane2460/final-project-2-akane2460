## Load Packages ----
library(tidyverse)
library(skimr)
library(janitor)
library(here)
library(knitr)
library(corrplot)

# load cleaned data
load(here("data/diabetic_mii.rda"))

# inspect data
skim_without_charts(diabetic_mii)

# distribution of risk/instability indices----
## single med
fivenum(diabetic_mii$single_med_risk_score)
diabetic_mii |> 
  ggplot(aes(x = single_med_risk_score)) +
  geom_boxplot()


## double med
fivenum(diabetic_mii$double_med_risk_score)
diabetic_mii |> 
  ggplot(aes(x = double_med_risk_score)) +
  geom_boxplot()


## mii
fivenum(diabetic_mii$medication_instability_index)
diabetic_mii |> 
  ggplot(aes(x = medication_instability_index)) +
  geom_boxplot()


# readmit rates
diabetic_mii |> 
  group_by(readmitted) |> 
  summarize(
        mean_mii = mean(medication_instability_index),
        mean_double_risk = mean(double_med_risk_score),
        mean_single_risk = mean(single_med_risk_score),
        n = n(),
        .groups = "drop"
      )

diabetic_mii |> 
  mutate(
    mii_exists = case_when(
    medication_instability_index != 0 ~ "Flagged",
    medication_instability_index == 0 ~ "Not Flagged",
  )) |> 
  group_by(mii_exists) |> 
    summarize(
      readmit_rate = mean(readmitted == "YES", na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )

diabetic_mii |> 
  mutate(
    mii_exists = case_when(
      medication_instability_index != 0 ~ "Flagged",
      medication_instability_index == 0 ~ "Not Flagged",
    )) |> 
  group_by(mii_exists, readmitted) |> 
  summarize(
    n = n(),
    .groups = "drop"
  )

# testing regression
mii_model <- glm(
  readmitted ~ medication_instability_index,
  data = diabetic_mii,
  family = binomial(link = "logit")
)

logit(P(diabetic_mii$readmit)) = β0 + β1 * diabetic_mii$medication_instability_index


