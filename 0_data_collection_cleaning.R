# Final project Data Cleaning ----
# Stat 301-1

## Load Packages ----

library(tidyverse)
library(skimr)
library(janitor)
library(here)
library(knitr)

# read in data
diabetic_data <- read_csv(here("data/diabetic_data.csv"))

# write out shortened version of data for memo 1
shortened_diabetic_data <- diabetic_data |> 
  head()

shortened_diabetic_data <- kable(shortened_diabetic_data, format = "html")

file_path_shortened_diabetic_data <- "results/shortened_diabetic_data.html"

writeLines(as.character(shortened_diabetic_data), con = file_path_shortened_diabetic_data)

# quality check
diabetic_data |> skimr::skim_without_charts()
  # they placed ? in place of NA for missing values
  diabetic_data |> filter(weight == "?")
  diabetic_data |> filter(weight != "?")
  # lots of "missing" data for weight
  
  diabetic_data |> filter(race == "?")
  diabetic_data |> filter(race != "?")
  # some missing data for race
  
  diabetic_data |> filter(payer_code == "?")
  diabetic_data |> filter(payer_code != "?")
  # lots of missigness for payer_code
  
  diabetic_data |> filter(medical_specialty == "?")
  diabetic_data |> filter(medical_specialty != "?")
  # lots of missigness for medical_specialty
  
  diabetic_data |> filter(diag_1 == "?")
  diabetic_data |> filter(diag_1 != "?")
  # very little missign for diag_1
  
  diabetic_data |> filter(diag_2 == "?")
  diabetic_data |> filter(diag_2 != "?")
  # very little missign for diag_2
  
  diabetic_data |> filter(diag_3 == "?")
  diabetic_data |> filter(diag_3 != "?")
  # very little missign for diag_3 (slightly more than 1 and 2)
  
  # no missigness otherwise
  
