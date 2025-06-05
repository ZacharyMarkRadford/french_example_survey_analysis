library(tidyverse)
library(brms)
library(tidybayes)

# Load frame --------------------------------------------------------------

population_dataset <- read_csv("data/population_data.csv")

# load models -------------------------------------------------------------

effort_model <- read_rds("models/effort_model.rds")
avidity_model <- read_rds("models/avidity_model.rds")
catch_model <- read_rds("models/catch_model.rds")

# Generate list of anglers ------------------------------------------------

draws_angler <- population_dataset |>
  add_epred_draws(effort_model,
                  value = "prob_angler") |>
  mutate(total_anglers = number_of_people * prob_angler) |>
  ungroup() |>
  select(gender, age, .draw, total_anglers)

draws_days_fished <- population_dataset |>
  add_predicted_draws(avidity_model,
                  value = "days_fished") |>
  ungroup() |>
  select(gender, age, .draw, days_fished)

draws_effort <- draws_angler |>
  left_join(draws_days_fished)

draws_catch <- draws_effort |>
  select(fishing_effort = days_fished) |>
  distinct() |>
  expand_grid(species = sort(unique(catch_model$data$species))) |>
  add_epred_draws(catch_model,
                  value = "pred_caught",
                  re_formula = ~ 1 # Removing diarist effect
                  ) |>
  ungroup() |>
  select(species, fishing_effort, .draw, pred_caught)

# Post-stratify survey ----------------------------------------------------

results <- draws_effort |>
  group_by(.draw, fishing_effort = days_fished) |>
  summarise(anglers = sum(total_anglers),
            .groups = "drop") |>
  left_join(draws_catch,
            relationship = "many-to-many") |>
  mutate(total_catch = anglers*pred_caught) |>
  group_by(species, .draw) |>
  summarise(anglers = sum(anglers),
            catch = sum(total_catch),
            .groups = "drop") |>
  group_by(species) |>
  summarise(total_anglers = mean(anglers),
            total_anglers_lower = quantile(anglers, 0.025),
            total_anglers_upper = quantile(anglers, 0.975),
            
            total_catch = mean(catch),
            total_catch_lower = quantile(catch, 0.025),
            total_catch_upper = quantile(catch, 0.975),
            .groups = "drop"
            ) |>
  mutate(anglers_rse = ((total_anglers_upper-total_anglers_lower)/3.92)/total_anglers,
         catch_rse = ((total_catch_upper-total_catch_lower)/3.92)/total_catch)

