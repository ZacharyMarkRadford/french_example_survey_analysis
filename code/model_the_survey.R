library(tidyverse)
library(brms)
library(tidybayes)

# Effort_survey -----------------------------------------------------------

effort_survey <- read_csv("data/effort_survey_data.csv")

effort_model <- brm(
  is_angler ~
    1 +
    gender +
    age,
  data = effort_survey,
  cores = 2,
  chains = 2, # You should use 8 cores and chains in reality, this is just an example
  iter = 2000,
  family = bernoulli(),
  file = "models/effort_model"
)

summary(effort_model)


# Days fished -------------------------------------------------------------

activity_model_data <- effort_survey |>
  filter(is_angler)

avidity_model <- brm(
  fishing_days | trials(365) ~ # 365 days in a year than can be fished
    1 +
    gender +
    age,
  data = activity_model_data,
  cores = 2,
  chains = 2, # You should use 8 cores and chains in reality, this is just an example
  iter = 2000,
  family = binomial(), # Should  use a beta_binomial as binomial can predict 0's, but it takes a bit longer and I didn't want to wait for this example
  file = "models/avidity_model"
)

# catch survey ------------------------------------------------------------

survey_data <- read_csv("data/survey_data.csv")

catch_model <- brm(
  number_fish_caught ~
    1 +
    fishing_effort +
    species +
    (1 | diarist_id),
  data = survey_data,
  cores = 2,
  chains = 2, # You should use 8 cores and chains in reality, this is just an example
  iter = 2000,
  family = poisson(),
  file = "models/catch_model"
)

summary(catch_model)

