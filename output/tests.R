# tests.R

library(ggplot2)
library(dplyr)
library(readxl)
library(lubridate)
library(glue)
source("run_model.R")
# Function to run the model with modified parameters
run_model_with_params <- function(param_Baseline, initialConditions, timesteps) {
  mo_baseline <- run_model.D(param_Baseline, initialConditions, timesteps)
  mo_baseline$moPostprocessing[[1]] |>
    mutate(age_group = as_factor(age_group))
}

# Function to test no fertility (nothing in M)
test_no_fertility <- function(param_Baseline, initialConditions, timesteps) {
  tryCatch({
    param_Baseline$births <- rep(0, length(param_Baseline$births))  # Set births to zero
    
    mop <- run_model_with_params(param_Baseline, initialConditions, timesteps)
    
    mop %>%
      filter(variable %in% "M") %>%
      ggplot() +
      aes(x = year, y = value, colour = age_group, group = age_group) +
      geom_line() +
      scale_color_hue(direction = 1) +
      theme_minimal() +
      labs(title = "No Fertility Test: M Compartment Over Time")
  }, error = function(e) {
    warning("Error in test_no_fertility: ", e$message)
    NULL
  })
}

# Function to test lambda at zero and vaccination off
test_lambda_zero_vaccination_off <- function(param_Baseline, initialConditions, timesteps) {
  tryCatch({
    param_Baseline$ptrans <- 0  # Set ptrans to zero to effectively set lambda to zero
    param_Baseline$v1[,] <- 0  # Turn off first vaccination
    param_Baseline$v2[,] <- 0  # Turn off second vaccination
    
    mop <- run_model_with_params(param_Baseline, initialConditions, timesteps)
    
    mop %>%
      filter(variable %in% c("S", "I", "R")) %>%
      ggplot() +
      aes(x = year, y = value, colour = age_group, group = age_group) +
      geom_line() +
      scale_color_hue(direction = 1) +
      theme_minimal() +
      facet_wrap(vars(variable), scales = "free_y") +
      labs(title = "Lambda Zero and Vaccination Off: S, I, R Compartments Over Time")
  }, error = function(e) {
    warning("Error in test_lambda_zero_vaccination_off: ", e$message)
    NULL
  })
}

# Function to test turning off second vaccination
test_turn_off_second_vaccination <- function(param_Baseline, initialConditions, timesteps) {
  tryCatch({
    param_Baseline$v2[,] <- 0  # Turn off second vaccination
    
    mop <- run_model_with_params(param_Baseline, initialConditions, timesteps)
    
    mop %>%
      filter(variable %in% c("V1p", "V1np")) %>%
      ggplot() +
      aes(x = year, y = value, colour = age_group, group = age_group) +
      geom_line() +
      scale_color_hue(direction = 1) +
      theme_minimal() +
      facet_wrap(vars(variable), scales = "free_y") +
      labs(title = "Turn Off Second Vaccination: V1p, V1np Compartments Over Time")
  }, error = function(e) {
    warning("Error in test_turn_off_second_vaccination: ", e$message)
    NULL
  })
}

# Function to check ages going into V1 and V2
test_ages_going_into_V1_V2 <- function(param_Baseline, initialConditions, timesteps) {
  tryCatch({
    mop <- run_model_with_params(param_Baseline, initialConditions, timesteps)
    
    mop %>%
      filter(variable %in% c("V1p", "V2p")) %>%
      ggplot() +
      aes(x = year, y = value, colour = age_group, group = age_group) +
      geom_line() +
      scale_color_hue(direction = 1) +
      theme_minimal() +
      facet_wrap(vars(variable), scales = "free_y") +
      labs(title = "Ages Going Into V1 and V2: V1p, V2p Compartments Over Time")
  }, error = function(e) {
    warning("Error in test_ages_going_into_V1_V2: ", e$message)
    NULL
  })
}

# Function to plot age profiles
test_age_profiles <- function(param_Baseline, initialConditions, timesteps) {
  tryCatch({
    mop <- run_model_with_params(param_Baseline, initialConditions, timesteps)
    
    mop %>%
      ggplot() +
      aes(x = year, y = value, colour = age_group, group = age_group) +
      geom_line() +
      scale_color_hue(direction = 1) +
      theme_minimal() +
      facet_wrap(vars(variable), scales = "free_y") +
      labs(title = "Age Profiles Over Time")
  }, error = function(e) {
    warning("Error in test_age_profiles: ", e$message)
    NULL
  })
}

# Function to plot rubella incidence
test_rubella_incidence <- function(param_Baseline, initialConditions, timesteps) {
  tryCatch({
    mop <- run_model_with_params(param_Baseline, initialConditions, timesteps)
    
    mop %>%
      filter(variable %in% "all_Inc_D") %>%
      ggplot() +
      aes(x = year, y = value, colour = age_group, group = age_group) +
      geom_line() +
      scale_color_hue(direction = 1) +
      theme_minimal() +
      labs(title = "Rubella Incidence Over Time")
  }, error = function(e) {
    warning("Error in test_rubella_incidence: ", e$message)
    NULL
  })
}

# Function to plot seroprevalence
test_seroprevalence <- function(param_Baseline, initialConditions, timesteps) {
  tryCatch({
    mop <- run_model_with_params(param_Baseline, initialConditions, timesteps)
    
    mop %>%
      filter(variable %in% "seroprevalence") %>%
      ggplot() +
      aes(x = year, y = value, colour = age_group, group = age_group) +
      geom_line() +
      scale_color_hue(direction = 1) +
      theme_minimal() +
      labs(title = "Seroprevalence Over Time")
  }, error = function(e) {
    warning("Error in test_seroprevalence: ", e$message)
    NULL
  })
}

# Function to plot over-time seroprevalence per age
test_overtime_seroprevalence_per_age <- function(param_Baseline, initialConditions, timesteps) {
  tryCatch({
    mop <- run_model_with_params(param_Baseline, initialConditions, timesteps)
    
    mop %>%
      filter(variable %in% "seroprevalence") %>%
      ggplot() +
      aes(x = year, y = value, colour = age_group, group = age_group) +
      geom_line() +
      scale_color_hue(direction = 1) +
      theme_minimal() +
      facet_wrap(vars(age_group), scales = "free_y") +
      labs(title = "Over-Time Seroprevalence Per Age Group")
  }, error = function(e) {
    warning("Error in test_overtime_seroprevalence_per_age: ", e$message)
    NULL
  })
}

# Function to check age groups (Neo-Nates, vaccinated, etc.)
test_check_age_groups <- function(param_Baseline, initialConditions, timesteps) {
  tryCatch({
    mop <- run_model_with_params(param_Baseline, initialConditions, timesteps)
    
    mop %>%
      filter(variable %in% c("Neo-Nates", "vaccinated")) %>%
      ggplot() +
      aes(x = year, y = value, colour = age_group, group = age_group) +
      geom_line() +
      scale_color_hue(direction = 1) +
      theme_minimal() +
      facet_wrap(vars(variable), scales = "free_y") +
      labs(title = "Checking Age Groups: Neo-Nates, Vaccinated, etc.")
  }, error = function(e) {
    warning("Error in test_check_age_groups: ", e$message)
    NULL
  })
}

# Main function to run all tests
run_all_tests <- function(param_Baseline, initialConditions, timesteps) {
  tests <- list()
  
  tests$no_fertility <- test_no_fertility(param_Baseline, initialConditions, timesteps)
  tests$lambda_zero_vaccination_off <- test_lambda_zero_vaccination_off(param_Baseline, initialConditions, timesteps)
  tests$turn_off_second_vaccination <- test_turn_off_second_vaccination(param_Baseline, initialConditions, timesteps)
  tests$ages_going_into_V1_V2 <- test_ages_going_into_V1_V2(param_Baseline, initialConditions, timesteps)
  tests$age_profiles <- test_age_profiles(param_Baseline, initialConditions, timesteps)
  tests$rubella_incidence <- test_rubella_incidence(param_Baseline, initialConditions, timesteps)
  tests$seroprevalence <- test_seroprevalence(param_Baseline, initialConditions, timesteps)
  tests$overtime_seroprevalence_per_age <- test_overtime_seroprevalence_per_age(param_Baseline, initialConditions, timesteps)
  tests$check_age_groups <- test_check_age_groups(param_Baseline, initialConditions, timesteps)
  
  return(tests)
}

# Example usage
# Assuming param_Baseline, initialConditions, and timesteps are already loaded in your environment
tests <- run_all_tests(param_Baseline, initialConditions, timesteps)

# Display tests
print(tests$no_fertility)
print(tests$lambda_zero_vaccination_off)
print(tests$turn_off_second_vaccination)
print(tests$ages_going_into_V1_V2)
print(tests$age_profiles)
print(tests$rubella_incidence)
print(tests$seroprevalence)
print(tests$overtime_seroprevalence_per_age)
print(tests$check_age_groups)
mop