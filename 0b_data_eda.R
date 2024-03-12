## Load Packages ----

library(tidyverse)
library(skimr)
library(janitor)
library(here)
library(knitr)
library(corrplot)

# read in training data 

# EDA----
  # load training data 
  load(here("data/diabetic_train.rda"))
  # load cleaned data
  load(here("data/diabetic_clean.rda"))

# univariate analysis----

  # categorical----
  
  # race
  race_demographics_plot <- diabetic_train |> 
    filter(race != "?") |> 
    ggplot(aes(x = race)) +
    geom_bar() +
    theme_minimal() +
    labs(
      title = "Racial Demographics of Diabetic Patients Admitted to Hospital",
      x = "Race"
    )
  
  ggsave(here("plots/race_demographics_plot.png"), race_demographics_plot, height = 8, width = 6, units = "in")
  
  # gender
  gender_demographics_plot <- diabetic_train |> 
    filter(gender != "?") |> 
    ggplot(aes(x = gender)) +
    geom_bar() +
    theme_minimal() +
    labs(
      title = "Gender Demographics of Diabetic Patients Admitted to Hospital",
      x = "Gender",
      y = ""
      )
  
  ggsave(here("plots/gender_demographics_plot.png"), gender_demographics_plot)
  
  # age
  age_demographics_plot <- diabetic_train |> 
    filter(age != "?") |> 
    ggplot(aes(x = age)) +
    geom_bar() +
    theme_minimal() +
    labs(
      title = "Age Demographics of Diabetic Patients Admitted to Hospital"
    )
  
  ggsave(here("plots/age_demographics_plot.png"), age_demographics_plot)
  
  # max_glu_serum
  diabetic_train |> 
    ggplot(aes(x = max_glu_serum)) +
    geom_bar() +
    theme_minimal()
  # "None" tends to outweigh most other categories
  
  # a1c 
  diabetic_train |> 
    ggplot(aes(x = a1cresult)) +
    geom_bar() +
    theme_minimal()
  # "None" most prominent category
      # however, for all other categories, ">8" was largest
      # indicating that when measured accurately patients typically had 
      # a higher than normal A1C 
  
  # change 
  diabetic_train |> 
    ggplot(aes(x = change)) +
    geom_bar() +
    theme_minimal()
    # "no" outweighs change, but numbers are fairly even
  
  # metformin 
  diabetic_train |> 
    ggplot(aes(x = metformin)) +
    geom_bar() +
    theme_minimal()
    # Most patients "No", "Steady" next top
      # could be relevant
  
  # repaglinide
  diabetic_train |> 
    ggplot(aes(x = repaglinide)) +
    geom_bar() +
    theme_minimal()
    # almost every patient "No", not worth exploring further
  
  # nateglinide
  diabetic_train |> 
    ggplot(aes(x = nateglinide)) +
    geom_bar() +
    theme_minimal()
    # almost every patient "No", not worth exploring further
  
  # chlorpropamide
  diabetic_train |> 
    ggplot(aes(x = chlorpropamide)) +
    geom_bar() +
    theme_minimal()
    # almost every patient "No", not worth exploring further
  
  # glimepiride 
  diabetic_train |> 
    ggplot(aes(x = glimepiride)) +
    geom_bar() +
    theme_minimal()
    # Most patients "No", "Steady" next top
    # could be relevant
  
  # acetohexamide
  diabetic_train |> 
    ggplot(aes(x = acetohexamide)) +
    geom_bar() +
    theme_minimal()
    # almost every patient "No", not worth exploring further
  
  # glipizide 
  diabetic_train |> 
    ggplot(aes(x = glipizide)) +
    geom_bar() +
    theme_minimal()
    # Most patients "No", "Steady" next top
    # could be relevant
  
  # glyburide 
  diabetic_train |> 
    ggplot(aes(x = glyburide)) +
    geom_bar() +
    theme_minimal()
    # Most patients "No", "Steady" next top
      # could be relevant
  
  # tolbutamide
  diabetic_train |> 
    ggplot(aes(x = tolbutamide)) +
    geom_bar() +
    theme_minimal()
    # almost every patient "No", not worth exploring further
  
  # pioglitazone 
  diabetic_train |> 
    ggplot(aes(x = pioglitazone)) +
    geom_bar() +
    theme_minimal()
    # Most patients "No", "Steady" next top
      # could be relevant
  
  # rosiglitazone 
  diabetic_train |> 
    ggplot(aes(x = rosiglitazone)) +
    geom_bar() +
    theme_minimal()
    # Most patients "No", "Steady" next top
      # could be relevant
    
  # acarbose
  diabetic_train |> 
    ggplot(aes(x = acarbose)) +
    geom_bar() +
    theme_minimal()
    # almost every patient "No", not worth exploring further
  
  # miglitol
  diabetic_train |> 
    ggplot(aes(x = miglitol)) +
    geom_bar() +
    theme_minimal()
    # almost every patient "No", not worth exploring further
  
  # troglitazone
  diabetic_train |> 
    ggplot(aes(x = troglitazone)) +
    geom_bar() +
    theme_minimal()
  # almost every patient "No", not worth exploring further
  
  # tolazamide
  diabetic_train |> 
    ggplot(aes(x = tolazamide)) +
    geom_bar() +
    theme_minimal()
    # almost every patient "No", not worth exploring further
  
  # examide
  diabetic_train |> 
    ggplot(aes(x = examide)) +
    geom_bar() +
    theme_minimal()
    # every patient "No", not worth exploring further
  
  # citoglipton
  diabetic_train |> 
    ggplot(aes(x = citoglipton)) +
    geom_bar() +
    theme_minimal()
    # every patient "No", not worth exploring further
  
  # insulin 
  diabetic_train |> 
    ggplot(aes(x = insulin)) +
    geom_bar() +
    theme_minimal()
    # "No" is most common, followed by "steady", "up" and "down"
  
  # glyburide_metformin
  diabetic_train |> 
    ggplot(aes(x = glyburide_metformin)) +
    geom_bar() +
    theme_minimal()
    # almost every patient "No", not worth exploring further
      # some patients "Steady" but a very small minority
      # not worth exploring further
  
  # glipizide_metformin
  diabetic_train |> 
    ggplot(aes(x = glipizide_metformin)) +
    geom_bar() +
    theme_minimal()
    # almost every patient "No", not worth exploring further
  
  # glimepiride_pioglitazone
  diabetic_train |> 
    ggplot(aes(x = glimepiride_pioglitazone)) +
    geom_bar() +
    theme_minimal()
    # almost every patient "No", not worth exploring further
  
  # metformin_rosiglitazone
  diabetic_train |> 
    ggplot(aes(x = metformin_rosiglitazone)) +
    geom_bar() +
    theme_minimal()
    # almost every patient "No", not worth exploring further
  
  # metformin_pioglitazone
  diabetic_train |> 
    ggplot(aes(x = metformin_pioglitazone)) +
    geom_bar() +
    theme_minimal()
    # almost every patient "No", not worth exploring further
  
  # diabetes_med 
  diabetic_train |> 
    ggplot(aes(x = diabetes_med)) +
    geom_bar() +
    theme_minimal()
      # MOST patients had a change in diabetes meds
      # but a condiserable amount didn't
  
  # categorical variables of interest:
    # based on these initial assessments, diabetes_med, insulin, rosiglitazone, 
    # pioglitazone, glyburide, glipizide, glimepiride, metformin, change, a1cresult,
    # max_glu_serum, age, race, gender all worth exploring as predictors
  
  # numeric----
  
  #  num_procedures
  diabetic_train |> 
    ggplot(aes(x = num_procedures)) +
    geom_boxplot() +
    theme_minimal()
  
  diabetic_train |> 
    ggplot(aes(x = num_procedures)) +
    geom_histogram() +
    theme_minimal()
  
  # skewed right with a peak at 0, but still substantial amount that have at least 1
  # median around 1
  
  # num_lab_procedures 
  diabetic_train |> 
    ggplot(aes(x = num_lab_procedures)) +
    geom_histogram() +
    theme_minimal()
  
  diabetic_train |> 
    summarize(
      med_num_lab_procedures = median(num_lab_procedures))
  
  # 2 major peaks, with most patients having 0 but almost even with around 50.
  
  # number_diagnoses 
  diabetic_train |> 
    ggplot(aes(x = number_diagnoses)) +
    geom_boxplot() +
    theme_minimal()
  
  diabetic_train |> 
    summarize(
      med_num_diagnoses = median(number_diagnoses))
  # MOST, if not all, patients received a diagnosis
    # typically 8 diagnoses (median value)
  
  # number_emergency 
  diabetic_train |> 
    ggplot(aes(x = number_emergency)) +
    geom_histogram() +
    theme_minimal()
  
  diabetic_train |> 
    summarize(
      med_num_emerg = median(number_emergency))
  # MOST patients had emergency visit 0 times leading up to hospitalization
      # does not seem to be worth exploring
  
  # time_in_hospital 
  diabetic_train |> 
    ggplot(aes(x = time_in_hospital)) +
    geom_boxplot() +
    theme_minimal()
  
  diabetic_train |> 
    summarize(
      med_time_in_hospital = median(time_in_hospital))
  # skewed right, peak appx 3 days, length of stay ranging from 1 to 14
    # typical (median) length of stay 4 days
  
  # number_outpatient  
  diabetic_train |> 
    ggplot(aes(x = number_outpatient)) +
    geom_histogram() +
    theme_minimal()
  
  diabetic_train |> 
    summarize(
      med_num_outpatient = median(number_outpatient))
  # MOST patients had outpatient visit 0 times leading up to hospitalization
      # still some with at least 1 
  
  # number inpatient 
  diabetic_train |> 
    ggplot(aes(x = number_inpatient)) +
    geom_histogram() +
    theme_minimal()
  
  diabetic_train |> 
    summarize(
      med_num_inpatient = median(number_inpatient))
    # MOST patients had inpatient visit 0 times leading up to hospitalization
    # still some with at least 1 
  
  # number medications
  diabetic_train |> 
    ggplot(aes(x = num_medications)) +
    geom_histogram() +
    theme_minimal()
  
  diabetic_train |> 
    summarize(
      med_num_medications = median(num_medications),
      min_num_med = min(num_medications),
      max_num_med = max(num_medications))
    # large range of medications taken (1 to 81)
    # typical number: 15 medications
    # very slight skew to right
  
  # numeric variables of interest:
  # based on these initial assessments, number_inpatient, number_outpatient, time_in_hospital, number_diagnoses, 
  # num_lab_procedures, num_medications, and num_procedures all worth exploring as predictors
  
  
# bivariate analyses----
  # determining potential confounds----
    # numeric x numeric predictors----
  
  diabetic_numeric <- diabetic_train |> 
    ungroup(readmitted) |> 
    select(where(is.numeric)) |> 
    select(number_inpatient, number_outpatient, time_in_hospital, number_diagnoses, 
           num_lab_procedures, num_procedures, num_medications) 
  
  correlation_matrix <- cor(diabetic_numeric)
  
  corrplot(correlation_matrix)
  # does not seem to be any major correlations between these variables
  # maybe a mild positive correlation between time_in_hospital and num_medications 
  # a mild positive correlation between time_in_hospital and num_lab_procedures
  # and a mild positive correlation between num_procedures and num_medications
  
    # categorical x numeric and categorical x categorical----
      # demographics----
      # age ----
  
    # potential confounds determined below: num_diagnoses, time_in_hospital,
    #  and number_inpatient
    
    # age and gender
    diabetic_train |> 
      ggplot(aes(x = age)) + 
      geom_bar() +
      facet_wrap(~ gender)
    
    # age and race
    diabetic_train |> 
      ggplot(aes(x = age)) + 
      geom_bar() +
      facet_wrap(~ race)
      # age distribution similar across gender and race
    
    # age and max_glu_serum
    diabetic_train |> 
      ggplot(aes(x = age)) + 
      geom_bar() +
      facet_wrap(~ max_glu_serum)
      # no major differences aside from different totals (most people have "None" as max_glu_serum)
    
    # age and a1c
    diabetic_train |> 
      ggplot(aes(x = age)) + 
      geom_bar() +
      facet_wrap(~ a1cresult)
      # no major differences aside from different totals (most people have "None" as A1C)
    
    # age and change
    diabetic_train |> 
      ggplot(aes(x = age)) + 
      geom_bar() +
      facet_wrap(~ change)
      # no major differences of age distribution among change categories
    
    # diabetes_med and age
    diabetic_train |> 
      ggplot(aes(x = age)) + 
      geom_bar() +
      facet_wrap(~ diabetes_med)
      # no major differences of age distribution among categories
    
    # insulin and age
    diabetic_train |> 
      ggplot(aes(x = age)) + 
      geom_bar() +
      facet_wrap(~ insulin)
      # no major differences of age distribution among categories
    
    # rosiglitazone and age
    diabetic_train |> 
      ggplot(aes(x = age)) + 
      geom_bar() +
      facet_wrap(~ rosiglitazone)
      # no major differences of age distribution among categories
    
    # pioglitazone and age
    diabetic_train |> 
      ggplot(aes(x = age)) + 
      geom_bar() +
      facet_wrap(~ pioglitazone)
      # no major differences of age distribution among categories
    
    # glyburide and age
    diabetic_train |> 
      ggplot(aes(x = age)) + 
      geom_bar() +
      facet_wrap(~ glyburide)
      # no major differences of age distribution among categories
    
    # glipizide and age 
    diabetic_train |> 
      ggplot(aes(x = age)) + 
      geom_bar() +
      facet_wrap(~ glipizide)
      # no major differences of age distribution among categories
    
    # glimepiride and age 
    diabetic_train |> 
      ggplot(aes(x = age)) + 
      geom_bar() +
      facet_wrap(~ glimepiride)
      # no major differences of age distribution among categories
    
    # metformin and age
    diabetic_train |> 
      ggplot(aes(x = age)) + 
      geom_bar() +
      facet_wrap(~ metformin)
      # no major differences of age distribution among categories
    
    # num_lab_procedures and age
    diabetic_train |> 
      ggplot(aes(x = num_procedures)) + 
      geom_boxplot() +
      facet_wrap(~ age)
    
    diabetic_train |> 
      group_by(age) |> 
      summarize(
        mean_lab_procedures = mean(num_procedures))
      # some differences, with those in middle age getting more procedures done
    
    # num_lab_procedures and age
    diabetic_train |> 
      ggplot(aes(x = num_lab_procedures)) + 
      geom_boxplot() +
      facet_wrap(~ age)
    
    diabetic_train |> 
      group_by(age) |> 
      summarize(
        med_lab_procedures = median(num_lab_procedures))
      # no major age differences
    
    # num_ diagnoses and age
    diabetic_train |> 
      ggplot(aes(x = number_diagnoses)) + 
      geom_boxplot() +
      facet_wrap(~ age)
    
    diabetic_train |> 
      group_by(age) |> 
      summarize(
        med_number_diagnoses = median(number_diagnoses))
      # generally see more diagnoses with older patients
      # POTENTIAL INTERACTION
    
    # time_in_hospital and age
    diabetic_train |> 
      ggplot(aes(x = time_in_hospital)) + 
      geom_boxplot() +
      facet_wrap(~ age)
    
    diabetic_train |> 
      group_by(age) |> 
      summarize(
        mean_time_in_hospital = mean(time_in_hospital),
        med_time_in_hospital = median(time_in_hospital))
      # generally see longer hospital stays with older patients
      # POTENTIAL INTERACTION
    
    # number_outpatient and age
    diabetic_train |> 
      ggplot(aes(x = number_outpatient)) + 
      geom_boxplot() +
      facet_wrap(~ age)
    
    diabetic_train |> 
      group_by(age) |> 
      summarize(
        mean_number_outpatient = mean(number_outpatient),
        med_number_outpatientl = median(number_outpatient))
      # no major differences here
    
    # number_inpatient and age
    diabetic_train |> 
      ggplot(aes(x = number_inpatient)) + 
      geom_boxplot() +
      facet_wrap(~ age)
    
    diabetic_train |> 
      group_by(age) |> 
      summarize(
        mean_number_inpatient = mean(number_inpatient),
        med_number_inpatient = median(number_inpatient))
      # some differences here, peaking in 20-30 year olds
      # and then tapering off
      # this could be relevant, with this age group being more at risk
      # potential interaction
    
      # gender----
    # potential interactions determined below: time_in_hospital
      # gender and race
      diabetic_train |> 
        ggplot(aes(x = gender)) + 
        geom_bar() +
        facet_wrap(~ race)
        # gender distribution similar across race
        # women typically outnumber men
      
      # gender and max_glu_serum
      diabetic_train |> 
        ggplot(aes(x = gender)) + 
        geom_bar() +
        facet_wrap(~ max_glu_serum)
        # no major differences aside from different totals (most people have "None" as max_glu_serum)
      
      # gender and a1c
      diabetic_train |> 
        ggplot(aes(x = gender)) + 
        geom_bar() +
        facet_wrap(~ a1cresult)
        # no major differences aside from different totals (most people have "None" as A1C)
      
      # gender and change
      diabetic_train |> 
        ggplot(aes(x = gender)) + 
        geom_bar() +
        facet_wrap(~ change)
        # no major differences of gender distribution among change categories
      
      # diabetes_med and gender
      diabetic_train |> 
        ggplot(aes(x = gender)) + 
        geom_bar() +
        facet_wrap(~ diabetes_med)
        # no major differences of gender distribution among categories
      
      # insulin and gender
      diabetic_train |> 
        ggplot(aes(x = gender)) + 
        geom_bar() +
        facet_wrap(~ insulin)
        # no major differences of gender distribution among categories
      
      # rosiglitazone and gender
      diabetic_train |> 
        ggplot(aes(x = gender)) + 
        geom_bar() +
        facet_wrap(~ rosiglitazone)
        # no major differences of gender distribution among categories
      
      # pioglitazone and gender
      diabetic_train |> 
        ggplot(aes(x = gender)) + 
        geom_bar() +
        facet_wrap(~ pioglitazone)
        # no major differences of gender distribution among categories
      
      # glyburide and gender
      diabetic_train |> 
        ggplot(aes(x = gender)) + 
        geom_bar() +
        facet_wrap(~ glyburide)
        # no major differences of gender distribution among categories
      
      # glipizide and gender 
      diabetic_train |> 
        ggplot(aes(x = gender)) + 
        geom_bar() +
        facet_wrap(~ glipizide)
        # no major differences of gender distribution among categories
      
      # glimepiride and gender 
      diabetic_train |> 
        ggplot(aes(x = gender)) + 
        geom_bar() +
        facet_wrap(~ glimepiride)
        # no major differences of gender distribution among categories
      
      # metformin and gender
      diabetic_train |> 
        ggplot(aes(x = gender)) + 
        geom_bar() +
        facet_wrap(~ metformin)
        # no major differences of gender distribution among categories
      
      # num_lab_procedures and gender
      diabetic_train |> 
        ggplot(aes(x = num_procedures)) + 
        geom_boxplot() +
        facet_wrap(~ gender)
      
      diabetic_train |> 
        group_by(gender) |> 
        summarize(
          med_lab_procedures = median(num_procedures))
        # no major differences
        
      # num_lab_procedures and gender
      diabetic_train |> 
        ggplot(aes(x = num_lab_procedures)) + 
        geom_boxplot() +
        facet_wrap(~ gender)
      
      diabetic_train |> 
        group_by(gender) |> 
        summarize(
          med_lab_procedures = median(num_lab_procedures))
      # no major gender differences
      
      # num_ diagnoses and gender 
      diabetic_train |> 
        ggplot(aes(x = number_diagnoses)) + 
        geom_boxplot() +
        facet_wrap(~ gender)
      
      diabetic_train |> 
        group_by(gender) |> 
        summarize(
          med_number_diagnoses = median(number_diagnoses))
      # no major gender differences
      
      # time_in_hospital and gender
      diabetic_train |> 
        ggplot(aes(x = time_in_hospital)) + 
        geom_boxplot() +
        facet_wrap(~ gender)
      
      diabetic_train |> 
        group_by(gender) |> 
        summarize(
          med_time_in_hospital = median(time_in_hospital))
      # generally see slight longer hospital stays with female patients
      # POTENTIAL INTERACTION?
      
      # number_outpatient and gender
      diabetic_train |> 
        ggplot(aes(x = number_outpatient)) + 
        geom_boxplot() +
        facet_wrap(~ gender)
      
      diabetic_train |> 
        group_by(gender) |> 
        summarize(
          med_number_outpatientl = median(number_outpatient))
      # no major differences here
      
      # number_inpatient and gender
      diabetic_train |> 
        ggplot(aes(x = number_inpatient)) + 
        geom_boxplot() +
        facet_wrap(~ gender)
      
      diabetic_train |> 
        group_by(gender) |> 
        summarize(
          med_number_inpatient = median(number_inpatient))
      # no major differences here
    
      # race----
      
      # potential confounds seen in: number_diagnoses
      
      # race and max_glu_serum
      diabetic_train |> 
        ggplot(aes(x = race)) + 
        geom_bar() +
        facet_wrap(~ max_glu_serum)
      # differences reflect racial make-up of the patient population 
      
      # race and a1c
      diabetic_train |> 
        ggplot(aes(x = race)) + 
        geom_bar() +
        facet_wrap(~ a1cresult)
      # differences reflect racial make-up of the patient population 

      # race and change
      diabetic_train |> 
        ggplot(aes(x = race)) + 
        geom_bar() +
        facet_wrap(~ change)
      # differences reflect racial make-up of the patient population 
      
      # diabetes_med and race
      diabetic_train |> 
        ggplot(aes(x = race)) + 
        geom_bar() +
        facet_wrap(~ diabetes_med)
      # differences reflect racial make-up of the patient population 
      
      # insulin and race
      diabetic_train |> 
        ggplot(aes(x = race)) + 
        geom_bar() +
        facet_wrap(~ insulin)
      # differences reflect racial make-up of the patient population 
      
      # rosiglitazone and race
      diabetic_train |> 
        ggplot(aes(x = race)) + 
        geom_bar() +
        facet_wrap(~ rosiglitazone)
      # differences reflect racial make-up of the patient population 
      
      # pioglitazone and race
      diabetic_train |> 
        ggplot(aes(x = race)) + 
        geom_bar() +
        facet_wrap(~ pioglitazone)
      # differences reflect racial make-up of the patient population 
      
      # glyburide and race
      diabetic_train |> 
        ggplot(aes(x = race)) + 
        geom_bar() +
        facet_wrap(~ glyburide)
      # differences reflect racial make-up of the patient population 
      
      # glipizide and race 
      diabetic_train |> 
        ggplot(aes(x = race)) + 
        geom_bar() +
        facet_wrap(~ glipizide)
      # differences reflect racial make-up of the patient population 
      
      # glimepiride and race 
      diabetic_train |> 
        ggplot(aes(x = race)) + 
        geom_bar() +
        facet_wrap(~ glimepiride)
      # differences reflect racial make-up of the patient population 
      
      # metformin and race
      diabetic_train |> 
        ggplot(aes(x = race)) + 
        geom_bar() +
        facet_wrap(~ metformin)
      # differences reflect racial make-up of the patient population 
      
      # num_lab_procedures and race 
      diabetic_train |> 
        ggplot(aes(x = num_procedures)) + 
        geom_boxplot() +
        facet_wrap(~ race)
      
      diabetic_train |> 
        group_by(race) |> 
        summarize(
          med_lab_procedures = median(num_procedures),
          mean_lab_procedures = mean(num_procedures))
      # no MAJOR differences
      
      # num_lab_procedures and race
      diabetic_train |> 
        ggplot(aes(x = num_lab_procedures)) + 
        geom_boxplot() +
        facet_wrap(~ race)
      
      diabetic_train |> 
        group_by(race) |> 
        summarize(
          med_lab_procedures = median(num_lab_procedures))
      # no major  differences
      
      # num_ diagnoses and race  
      diabetic_train |> 
        ggplot(aes(x = number_diagnoses)) + 
        geom_boxplot() +
        facet_wrap(~ race)
      
      diabetic_train |> 
        group_by(race) |> 
        summarize(
          med_number_diagnoses = median(number_diagnoses))
      # white patients do tend to receive the most diagnoses
        # out of any other racial group
          # slight differences more critical
            # if non-Caucasian patients underdiagnosed, that is important
            # to note even if it is just by 1-2 diagnoses
          # POTENTIAL INTERACTION
      
      # time_in_hospital and race
      diabetic_train |> 
        ggplot(aes(x = time_in_hospital)) + 
        geom_boxplot() +
        facet_wrap(~ race)
      
      diabetic_train |> 
        group_by(race) |> 
        summarize(
          med_time_in_hospital = median(time_in_hospital))
      # no MAJOR differences 
      
      # number_outpatient and race
      diabetic_train |> 
        ggplot(aes(x = number_outpatient)) + 
        geom_boxplot() +
        facet_wrap(~ race)
      
      diabetic_train |> 
        group_by(race) |> 
        summarize(
          med_number_outpatientl = median(number_outpatient))
      # no major differences here
      
      # number_inpatient and race
      diabetic_train |> 
        ggplot(aes(x = number_inpatient)) + 
        geom_boxplot() +
        facet_wrap(~ race)
      
      diabetic_train |> 
        group_by(race) |> 
        summarize(
          med_number_inpatient = median(number_inpatient))
      # no major differences here
    
      
      # medications/lab results----
        # considering only major medications and lab results
      # max_glu_serum----
      
      # potential confounds/interactions include: num_lab_procedures
      
      # num_lab_procedures and max_glu_serum 
      diabetic_train |> 
        ggplot(aes(x = num_procedures)) + 
        geom_boxplot() +
        facet_wrap(~ max_glu_serum)
      
      diabetic_train |> 
        group_by(max_glu_serum) |> 
        summarize(
          med_lab_procedures = median(num_procedures),
          mean_lab_procedures = mean(num_procedures))
      # No MAJOR differences
      
      # num_lab_procedures and max_glu_serum
      diabetic_train |> 
        ggplot(aes(x = num_lab_procedures)) + 
        geom_boxplot() +
        facet_wrap(~ max_glu_serum)
      
      diabetic_train |> 
        group_by(max_glu_serum) |> 
        summarize(
          med_lab_procedures = median(num_lab_procedures))
      # those who did not have the test done typically had much higher
      # number of lab procedures
      # POTENTIAL INTERACTION
      
      # num_ diagnoses and max_glu_serum  
      diabetic_train |> 
        ggplot(aes(x = number_diagnoses)) + 
        geom_boxplot() +
        facet_wrap(~ max_glu_serum)
      
      diabetic_train |> 
        group_by(max_glu_serum) |> 
        summarize(
          med_number_diagnoses = median(number_diagnoses))
      # no MAJOR differences
      
      # time_in_hospital and max_glu_serum
      diabetic_train |> 
        ggplot(aes(x = time_in_hospital)) + 
        geom_boxplot() +
        facet_wrap(~ max_glu_serum)
      
      diabetic_train |> 
        group_by(max_glu_serum) |> 
        summarize(
          med_time_in_hospital = median(time_in_hospital))
      # no MAJOR differences
      
      # number_outpatient and max_glu_serum
      diabetic_train |> 
        ggplot(aes(x = number_outpatient)) + 
        geom_boxplot() +
        facet_wrap(~ max_glu_serum)
      
      diabetic_train |> 
        group_by(max_glu_serum) |> 
        summarize(
          med_number_outpatientl = median(number_outpatient))
      # no major differences here
      
      # number_inpatient and max_glu_serum
      diabetic_train |> 
        ggplot(aes(x = number_inpatient)) + 
        geom_boxplot() +
        facet_wrap(~ max_glu_serum)
      
      diabetic_train |> 
        group_by(max_glu_serum) |> 
        summarize(
          med_number_inpatient = median(number_inpatient))
      # no major differences here
      
      
    
      # a1cresult----
      
      # potential confounds/interactions include: num_lab_procedures
      
      # num_procedures and a1cresult
      diabetic_train |> 
        ggplot(aes(x = num_procedures)) + 
        geom_boxplot() +
        facet_wrap(~ a1cresult)
      
      diabetic_train |> 
        group_by(a1cresult) |> 
        summarize(
          med_procedures = median(num_procedures),
          mean_procedures = mean(num_procedures))
      # no MAJOR differences
      
      # num_lab_procedures and change
      diabetic_train |> 
        ggplot(aes(x = num_lab_procedures)) + 
        geom_boxplot() +
        facet_wrap(~ a1cresult)
      
      diabetic_train |> 
        group_by(a1cresult) |> 
        summarize(
          med_lab_procedures = median(num_lab_procedures))
      # those that didn't have the A1C test had much lower
      # POTENTIAL INTERACTION
      
      # num_ diagnoses and a1cresult  
      diabetic_train |> 
        ggplot(aes(x = number_diagnoses)) + 
        geom_boxplot() +
        facet_wrap(~ a1cresult)
      
      diabetic_train |> 
        group_by(a1cresult) |> 
        summarize(
          med_number_diagnoses = median(number_diagnoses))
      # no MAJOR differences
      
      # time_in_hospital and a1cresult
      diabetic_train |> 
        ggplot(aes(x = time_in_hospital)) + 
        geom_boxplot() +
        facet_wrap(~ a1cresult)
      
      diabetic_train |> 
        group_by(a1cresult) |> 
        summarize(
          med_time_in_hospital = median(time_in_hospital))
      # no MAJOR differences
      
      # number_outpatient and a1cresult
      diabetic_train |> 
        ggplot(aes(x = number_outpatient)) + 
        geom_boxplot() +
        facet_wrap(~ a1cresult)
      
      diabetic_train |> 
        group_by(a1cresult) |> 
        summarize(
          med_number_outpatientl = median(number_outpatient))
      # no major differences here
      
      # number_inpatient and a1cresult
      diabetic_train |> 
        ggplot(aes(x = number_inpatient)) + 
        geom_boxplot() +
        facet_wrap(~ a1cresult)
      
      diabetic_train |> 
        group_by(a1cresult) |> 
        summarize(
          med_number_inpatient = median(number_inpatient))
      # no major differences here
      # change----
      # no major interactions
      # num_procedures and change
      diabetic_train |> 
        ggplot(aes(x = num_procedures)) + 
        geom_boxplot() +
        facet_wrap(~ change)
      
      diabetic_train |> 
        group_by(change) |> 
        summarize(
          med_procedures = median(num_procedures),
          mean_procedures = mean(num_procedures))
      # no MAJOR differences
      
      # num_lab_procedures and change
      diabetic_train |> 
        ggplot(aes(x = num_lab_procedures)) + 
        geom_boxplot() +
        facet_wrap(~ change)
      
      diabetic_train |> 
        group_by(change) |> 
        summarize(
          med_lab_procedures = median(num_lab_procedures))
      # no MAJOR differences
      # 2 procedure difference, not super substantial
      
      # num_ diagnoses and change  
      diabetic_train |> 
        ggplot(aes(x = number_diagnoses)) + 
        geom_boxplot() +
        facet_wrap(~ change)
      
      diabetic_train |> 
        group_by(change) |> 
        summarize(
          med_number_diagnoses = median(number_diagnoses))
      # no MAJOR differences
      
      # time_in_hospital and change
      diabetic_train |> 
        ggplot(aes(x = time_in_hospital)) + 
        geom_boxplot() +
        facet_wrap(~ change)
      
      diabetic_train |> 
        group_by(change) |> 
        summarize(
          med_time_in_hospital = median(time_in_hospital))
      # no MAJOR differences
      # 1 day difference, not super substantial
      
      # number_outpatient and change
      diabetic_train |> 
        ggplot(aes(x = number_outpatient)) + 
        geom_boxplot() +
        facet_wrap(~ change)
      
      diabetic_train |> 
        group_by(change) |> 
        summarize(
          med_number_outpatientl = median(number_outpatient))
      # no major differences here
      
      # number_inpatient and change
      diabetic_train |> 
        ggplot(aes(x = number_inpatient)) + 
        geom_boxplot() +
        facet_wrap(~ change)
      
      diabetic_train |> 
        group_by(change) |> 
        summarize(
          med_number_inpatient = median(number_inpatient))
      # no major differences here
      # diabetes_med----
      
      # no major interactions
      
      # num_procedures and diabetes_med
      diabetic_train |> 
        ggplot(aes(x = num_procedures)) + 
        geom_boxplot() +
        facet_wrap(~ diabetes_med)
      
      diabetic_train |> 
        group_by(diabetes_med) |> 
        summarize(
          med_procedures = median(num_procedures),
          mean_procedures = mean(num_procedures))
      # no MAJOR differences
      
      # num_lab_procedures and diabetes_med
      diabetic_train |> 
        ggplot(aes(x = num_lab_procedures)) + 
        geom_boxplot() +
        facet_wrap(~ diabetes_med)
      
      diabetic_train |> 
        group_by(diabetes_med) |> 
        summarize(
          med_lab_procedures = median(num_lab_procedures))
      # no MAJOR differences
        # 2 procedure difference, not super substantial
      
      # num_ diagnoses and diabetes_med  
      diabetic_train |> 
        ggplot(aes(x = number_diagnoses)) + 
        geom_boxplot() +
        facet_wrap(~ diabetes_med)
      
      diabetic_train |> 
        group_by(diabetes_med) |> 
        summarize(
          med_number_diagnoses = median(number_diagnoses))
      # no MAJOR differences
      
      # time_in_hospital and diabetes_med
      diabetic_train |> 
        ggplot(aes(x = time_in_hospital)) + 
        geom_boxplot() +
        facet_wrap(~ diabetes_med)
      
      diabetic_train |> 
        group_by(diabetes_med) |> 
        summarize(
          med_time_in_hospital = median(time_in_hospital))
      # no MAJOR differences
          # 1 day difference, not super substantial
      
      # number_outpatient and diabetes_med
      diabetic_train |> 
        ggplot(aes(x = number_outpatient)) + 
        geom_boxplot() +
        facet_wrap(~ diabetes_med)
      
      diabetic_train |> 
        group_by(diabetes_med) |> 
        summarize(
          med_number_outpatientl = median(number_outpatient))
      # no major differences here
      
      # number_inpatient and diabetes_med
      diabetic_train |> 
        ggplot(aes(x = number_inpatient)) + 
        geom_boxplot() +
        facet_wrap(~ diabetes_med)
      
      diabetic_train |> 
        group_by(diabetes_med) |> 
        summarize(
          med_number_inpatient = median(number_inpatient))
      # no major differences here
      # insulin----
      
      # potential interactions: num_lab_procedures
      
      # num_procedures and insulin
      diabetic_train |> 
        ggplot(aes(x = num_procedures)) + 
        geom_boxplot() +
        facet_wrap(~ insulin)
      
      diabetic_train |> 
        group_by(insulin) |> 
        summarize(
          med_procedures = median(num_procedures),
          mean_procedures = mean(num_procedures))
      # no MAJOR differences
      
      # num_lab_procedures and insulin
      diabetic_train |> 
        ggplot(aes(x = num_lab_procedures)) + 
        geom_boxplot() +
        facet_wrap(~ insulin)
      
      diabetic_train |> 
        group_by(insulin) |> 
        summarize(
          med_lab_procedures = median(num_lab_procedures))
      # those who needed to "Down" "Up" their insulin saw higher number of lab procedures
      # POTENTIAL INTERACTION
      
      # num_ diagnoses and insulin  
      diabetic_train |> 
        ggplot(aes(x = number_diagnoses)) + 
        geom_boxplot() +
        facet_wrap(~ insulin)
      
      diabetic_train |> 
        group_by(insulin) |> 
        summarize(
          med_number_diagnoses = median(number_diagnoses))
      # no MAJOR differences
      
      # time_in_hospital and insulin
      diabetic_train |> 
        ggplot(aes(x = time_in_hospital)) + 
        geom_boxplot() +
        facet_wrap(~ insulin)
      
      diabetic_train |> 
        group_by(insulin) |> 
        summarize(
          med_time_in_hospital = median(time_in_hospital))
      # no MAJOR differences
      
      # number_outpatient and insulin
      diabetic_train |> 
        ggplot(aes(x = number_outpatient)) + 
        geom_boxplot() +
        facet_wrap(~ insulin)
      
      diabetic_train |> 
        group_by(insulin) |> 
        summarize(
          med_number_outpatientl = median(number_outpatient))
      # no major differences here
      
      # number_inpatient and insulin
      diabetic_train |> 
        ggplot(aes(x = number_inpatient)) + 
        geom_boxplot() +
        facet_wrap(~ insulin)
      
      diabetic_train |> 
        group_by(insulin) |> 
        summarize(
          med_number_inpatient = median(number_inpatient))
      # no major differences here
      # metformin----
      
      # potential interactions: num_lab_procedures
      
      # num_procedures and metformin
      diabetic_train |> 
        ggplot(aes(x = num_procedures)) + 
        geom_boxplot() +
        facet_wrap(~ metformin)
      
      diabetic_train |> 
        group_by(metformin) |> 
        summarize(
          med_procedures = median(num_procedures),
          mean_procedures = mean(num_procedures))
      # no MAJOR differences
      
      # num_lab_procedures and metformin
      diabetic_train |> 
        ggplot(aes(x = num_lab_procedures)) + 
        geom_boxplot() +
        facet_wrap(~ metformin)
      
      diabetic_train |> 
        group_by(metformin) |> 
        summarize(
          med_lab_procedures = median(num_lab_procedures))
      # those who needed to "Up" their metformin saw higher number of lab procedures
      # POTENTIAL INTERACTION
      
      # num_ diagnoses and metformin  
      diabetic_train |> 
        ggplot(aes(x = number_diagnoses)) + 
        geom_boxplot() +
        facet_wrap(~ metformin)
      
      diabetic_train |> 
        group_by(metformin) |> 
        summarize(
          med_number_diagnoses = median(number_diagnoses))
      # no MAJOR differences
      
      # time_in_hospital and metformin
      diabetic_train |> 
        ggplot(aes(x = time_in_hospital)) + 
        geom_boxplot() +
        facet_wrap(~ metformin)
      
      diabetic_train |> 
        group_by(metformin) |> 
        summarize(
          med_time_in_hospital = median(time_in_hospital))
      # no MAJOR differences
          # patients who are "UP" generally stay 1 day longer
      
      # number_outpatient and metformin
      diabetic_train |> 
        ggplot(aes(x = number_outpatient)) + 
        geom_boxplot() +
        facet_wrap(~ metformin)
      
      diabetic_train |> 
        group_by(metformin) |> 
        summarize(
          med_number_outpatientl = median(number_outpatient))
      # no major differences here
      
      # number_inpatient and metformin
      diabetic_train |> 
        ggplot(aes(x = number_inpatient)) + 
        geom_boxplot() +
        facet_wrap(~ metformin)
      
      diabetic_train |> 
        group_by(metformin) |> 
        summarize(
          med_number_inpatient = median(number_inpatient))
      # no major differences here
      
  # target variable analysis----
      # univariate----
        diabetic_train |> skimr::skim_without_charts(readmitted)
        
        readmitted_plot <- diabetic_train |> 
          ggplot(aes(x = readmitted)) +
          geom_bar() +
          theme_minimal() +
          labs(
            title = "Diabetic Patients: Readmission to Hospital Frequency",
            x = "Readmitted",
            y = ""
          )
        
        ggsave(here("memos/results/readmitted_plot.png"), readmitted_plot)
        ggsave(here("plots/readmitted_plot.png"), readmitted_plot, height = 8, width = 6, units = "in")
        
        readmitted_table <- diabetic_clean |> 
          summarize(
            not_readmitted = sum(readmitted == "NO"),
            readmitted = sum(readmitted != "NO"),
            Percent_No = not_readmitted / (not_readmitted + readmitted) * 100,
            Percent_Yes = readmitted / (not_readmitted + readmitted) * 100,
          ) |> 
          select(Percent_No, Percent_Yes)
        
        readmitted_table <- knitr::kable(readmitted_table)
        
      # bivariate----
        # age and readmitted
        age_readmitted_plot <- diabetic_train |> 
          ggplot(aes(x = readmitted)) + 
          geom_bar() +
          facet_wrap(~ age) +
          labs( title = "Age of Diabetic Patients Readmitted to Hospital",
                x = "Readmitted")
        
        ggsave(here("plots/age_readmitted_plot.png"), age_readmitted_plot)
        
        # readmitted and gender
        gender_readmitted_plot <- diabetic_train |> 
          ggplot(aes(x = readmitted)) + 
          geom_bar() +
          facet_wrap(~ gender) +
          labs( title = "Gender of Diabetic Patients Readmitted to Hospital",
                x = "Readmitted")
        
        ggsave(here("plots/gender_readmitted_plot.png"), gender_readmitted_plot)
        
        # readmitted and race
        race_readmitted_plot <- diabetic_train |> 
          ggplot(aes(x = readmitted)) + 
          geom_bar() +
          facet_wrap(~ race) +
          labs( title = "Race of Diabetic Patients Readmitted to Hospital",
                                            x = "Readmitted")
        
        ggsave(here("plots/race_readmitted_plot.png"), race_readmitted_plot)
        
        # readmitted and max_glu_serum
        diabetic_train |> 
          ggplot(aes(x = readmitted)) + 
          geom_bar() +
          facet_wrap(~ max_glu_serum)

        # readmitted and a1c
        diabetic_train |> 
          ggplot(aes(x = readmitted)) + 
          geom_bar() +
          facet_wrap(~ a1cresult)

        # readmitted and change
        diabetic_train |> 
          ggplot(aes(x = readmitted)) + 
          geom_bar() +
          facet_wrap(~ change)
        
        # diabetes_med and readmitted
        diabetic_train |> 
          ggplot(aes(x = readmitted)) + 
          geom_bar() +
          facet_wrap(~ diabetes_med)
        
        # insulin and readmitted
        diabetic_train |> 
          ggplot(aes(x = readmitted)) + 
          geom_bar() +
          facet_wrap(~ insulin)
        
        # rosiglitazone and readmitted
        diabetic_train |> 
          ggplot(aes(x = readmitted)) + 
          geom_bar() +
          facet_wrap(~ rosiglitazone)
        
        # pioglitazone and readmitted
        diabetic_train |> 
          ggplot(aes(x = readmitted)) + 
          geom_bar() +
          facet_wrap(~ pioglitazone)
        
        # glyburide and readmitted
        diabetic_train |> 
          ggplot(aes(x = readmitted)) + 
          geom_bar() +
          facet_wrap(~ glyburide)
        
        # glipizide and readmitted 
        diabetic_train |> 
          ggplot(aes(x = readmitted)) + 
          geom_bar() +
          facet_wrap(~ glipizide)
        
        # glimepiride and readmitted 
        diabetic_train |> 
          ggplot(aes(x = readmitted)) + 
          geom_bar() +
          facet_wrap(~ glimepiride)
        
        # metformin and readmitted
        diabetic_train |> 
          ggplot(aes(x = readmitted)) + 
          geom_bar() +
          facet_wrap(~ metformin)
        
        # num_lab_procedures and readmitted
        diabetic_train |> 
          ggplot(aes(x = num_procedures)) + 
          geom_boxplot() +
          facet_wrap(~ readmitted)
        
        diabetic_train |> 
          group_by(readmitted) |> 
          summarize(
            mean_lab_procedures = mean(num_procedures))

        # num_lab_procedures and readmitted
        diabetic_train |> 
          ggplot(aes(x = num_lab_procedures)) + 
          geom_boxplot() +
          facet_wrap(~ readmitted)
        
        diabetic_train |> 
          group_by(readmitted) |> 
          summarize(
            med_lab_procedures = median(num_lab_procedures))
        
        # num_ diagnoses and readmitted
        diabetic_train |> 
          ggplot(aes(x = number_diagnoses)) + 
          geom_boxplot() +
          facet_wrap(~ readmitted)
        
        diabetic_train |> 
          group_by(readmitted) |> 
          summarize(
            med_number_diagnoses = median(number_diagnoses))
        
        # time_in_hospital and readmitted
        diabetic_train |> 
          ggplot(aes(x = time_in_hospital)) + 
          geom_boxplot() +
          facet_wrap(~ readmitted)
        
        diabetic_train |> 
          group_by(readmitted) |> 
          summarize(
            mean_time_in_hospital = mean(time_in_hospital),
            med_time_in_hospital = median(time_in_hospital))
          # generally see longer hospital stays with those readmitted earlier
        
        # number_outpatient and readmitted
        diabetic_train |> 
          ggplot(aes(x = number_outpatient)) + 
          geom_boxplot() +
          facet_wrap(~ readmitted)
        
        diabetic_train |> 
          group_by(readmitted) |> 
          summarize(
            mean_number_outpatient = mean(number_outpatient),
            med_number_outpatientl = median(number_outpatient))
        
        # number_inpatient and readmitted
        diabetic_train |> 
          ggplot(aes(x = number_inpatient)) + 
          geom_boxplot() +
          facet_wrap(~ readmitted)
        
        diabetic_train |> 
          group_by(readmitted) |> 
          summarize(
            mean_number_inpatient = mean(number_inpatient),
            med_number_inpatient = median(number_inpatient))

# conclusions----
    # overall, problematic variables or ones with little to no variation have been identified
        # ones that are not relevant to the prediction problem 
        # or have logical issues (ex: weight)
        # are to be removed from our analysis
    # relevant predictors include:
        # number_inpatient, number_outpatient, time_in_hospital, number_diagnoses, 
        # num_lab_procedures, num_medications, num_procedures, diabetes_med, 
        # insulin, rosiglitazone, pioglitazone, glyburide, glipizide, glimepiride, 
        # metformin, change, a1cresult, max_glu_serum, age, race, gender all worth exploring as predictors
    # no one predictor reliable for assessing readmit risk alone, therefore they 
        # must be examined in combination
    # potential confounding effects or interactions between: 
        # time_in_hospital and num_medications, num_lab_procedures
        # num_procedures and num_medications
        # age and num_diagnoses, time_in_hospital, number_inpatient
        # gender and time_in_hospital
        # race and number_diagnoses
        # num_lab_procedures and max_glu_serum, a1cresult, insulin, metformin
        # will examine how these interactions affect model accuracy

        
