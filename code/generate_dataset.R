library(tidyverse)
library(brms)
library(tidybayes)

set.seed(123)

# Catch survey ------------------------------------------------------------

# Number of diarists
n_diarists <- 50

# Diarist IDs
diarist_id <- paste0("D", sprintf("%03d", 1:n_diarists))

# Diarist-specific fishing effort (number of trips in a year)
fishing_effort <- rpois(n_diarists, lambda = 15) + 1  # Ensuring no zero effort

# Species names
species <- c("Species_A", "Species_B", "Species_C", "Species_D", "Species_E")

# Base catch rates per species (relative catch rates)
species_catch_rate <- c(1.2, 1.0, 0.8, 0.6, 0.4)

# Generate diarist-specific random effects for each species

# Initialize empty dataframe
survey_data <- expand.grid(diarist_id = diarist_id,
                           species = species,
                           stringsAsFactors = FALSE)

# Assign diarist-specific random effects
survey_data <- survey_data |>
  mutate(diarist_effect = rnorm(n(), mean = 0, sd = 0.3))

# Add fishing effort to each diarist
survey_data <- survey_data |>
  left_join(data.frame(diarist_id, fishing_effort), by = "diarist_id")

# Calculate catch rates
survey_data <- survey_data |>
  mutate(species_base_rate = rep(species_catch_rate, each = n_diarists),
         lambda_catch = exp(log(fishing_effort) + log(species_base_rate) + diarist_effect),
         number_fish_caught = rpois(n(), lambda = lambda_catch)) |>
  select(diarist_id, fishing_effort, species, number_fish_caught)

write_csv(survey_data, "data/survey_data.csv")

# View the data
head(survey_data)


# Effort survey -----------------------------------------------------------
# Number of people in the survey
n_people <- 5000

# Generate effort IDs
effort_id <- 1:n_people

# Generate gender distribution (50/50 male/female)
gender <- sample(c("Male", "Female"), n_people, replace = TRUE)

# Generate age uniformly from 16 to 99
age <- sample(16:99, n_people, replace = TRUE)

# Logistic function parameters for being an angler
# Adjusted so overall angler proportion is about 5%
intercept <- -4
coef_gender <- 0.5      # Males more likely
coef_age <- 0.03        # Older more likely

# Calculate probability of being an angler
logit_p <- intercept + coef_gender * (gender == "Male") + coef_age * age
p_angler <- exp(logit_p) / (1 + exp(logit_p))

# Generate angler status
is_angler <- rbinom(n_people, size = 1, prob = p_angler) == 1

# Generate fishing days (0 for non-anglers, correlated with age and gender for anglers)
fishing_days <- ifelse(is_angler,
                       pmin(365, round(rpois(n_people, lambda = (age / 20 + 2 * (gender == "Male")) * 5) + 1)),
                       0)

# Combine into a dataframe
dataset_angler <- data.frame(
  effort_id = effort_id,
  gender = gender,
  age = age,
  is_angler = is_angler,
  fishing_days = fishing_days
)


write_csv(dataset_angler, "data/effort_survey_data.csv")

# Population data ---------------------------------------------------------

# Total population size
pop_size <- 1e6

# Generate age distribution (assuming uniform for simplicity)
age <- sample(16:99, pop_size, replace = TRUE)

# Generate gender distribution (50/50)
gender <- sample(c("Male", "Female"), pop_size, replace = TRUE)

# Combine into a dataframe
poststrat_population <- data.frame(
  age = age,
  gender = gender
)

poststrat_population <- poststrat_population |>
  group_by(gender, age) |>
  summarise(number_of_people = n(),
            .groups = "drop")

write_csv(poststrat_population, "data/population_data.csv")
