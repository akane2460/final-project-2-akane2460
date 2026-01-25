## Load Packages ----
library(tidyverse)
library(skimr)
library(janitor)
library(here)
library(knitr)
library(corrplot)

# load cleaned data
load(here("data/diabetic_clean.rda"))

## general readmission----
general_readmit_rates <- diabetic_clean |>
  group_by(readmitted) |> 
  summarize(
    n = n(),
    readmit_rate = n()/96921,
    .groups = "drop"
  )

cols <- c("Not Readmitted" = "#4CB04C", "Readmitted" = "#B04C4C")

readmit_rates_plot <- general_readmit_rates |>
  mutate(readmitted = recode(readmitted,
                             NO = "Not Readmitted",
                             YES = "Readmitted")) |> 
  ggplot(aes(x = readmitted, y = readmit_rate, fill = readmitted)) +
  geom_col() +
  geom_text(
    aes(
      x = readmitted,
      y = readmit_rate,
      label = scales::percent(readmit_rate, accuracy = 0.1)),
    vjust = -0.5,
    size = 4
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = cols) +
  labs(
    title = "30-Day Hospital Readmission Rates",
    x = "Readmission Status",
    y = "30-Day Readmission Rate",
  ) +
  guides(fill = "none") +
  theme_minimal()

ggsave("additional_analyses/plots/readmit_rates_plot.png", plot = readmit_rates_plot)


## age----
diabetic_clean |> 
  # group_by(age, readmitted) |>
  filter(readmitted == 'YES') |> 
  group_by(age) |>
  summarise(
    n = n()
  )
  # about 60% of readmitted patients eligible for medicare

## duration of stay----
diabetic_clean |> 
  # group_by(readmitted) |> 
  ggplot(aes(x = time_in_hospital)) +
  geom_histogram() +
  facet_wrap(~ readmitted)

readmitted_LOS_plot <- diabetic_clean |> 
  group_by(readmitted) |> 
  summarise(
    avg_length_of_stay = mean(time_in_hospital, na.rm = TRUE),
    median_length_of_stay = median(time_in_hospital, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) |> 
  mutate(readmitted = recode(readmitted,
                             NO = "Not Readmitted",
                             YES = "Readmitted")) |> 
  ggplot(aes(x = median_length_of_stay, y = readmitted, fill = readmitted)) +
  geom_col() +
  scale_fill_manual(values = cols) +
  labs(
    title = "Length of Hospital Stay Across 30-Day Readmission Status",
    x = "Median Length of Stay (Days)",
    y = "Readmission Status",
  ) +
  guides(fill = "none") +
  theme_minimal()

ggsave("additional_analyses/plots/readmitted_LOS_plot.png", plot = readmitted_LOS_plot, width = 6.5, height = 6)


## medications----



