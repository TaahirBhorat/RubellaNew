# plots_fn.R

library(ggplot2)
library(dplyr)
library(gridExtra)
library(readxl)
library(lubridate)
library(glue)
library(haven)

# Function to read and process model output
read_model_output <- function(mo_baseline) {
  mo_baseline$moPostprocessing[[1]] |>
    mutate(age_group = as_factor(age_group))
}

# Function to plot death compartments
plot_death_compartments <- function(mop) {
  tryCatch({
    mop %>%
      filter(variable %in% "deaths_D") %>%
      ggplot() +
      aes(x = year, y = value, colour = age_group, group = age_group) +
      geom_line() +
      scale_color_hue(direction = 1) +
      theme_minimal() +
      facet_wrap(vars(variable), scales = "free_y")
  }, error = function(e) {
    warning("Error in plot_death_compartments: ", e$message)
    NULL
  })
}

# Function to read true death data
read_true_deaths <- function(file_path) {
  tryCatch({
    read_excel(file_path, sheet = "totdeaths")[,-1]
  }, error = function(e) {
    warning("Error in read_true_deaths: ", e$message)
    NULL
  })
}

# Function to process raw model data
process_raw_model_data <- function(mo_baseline) {
  tryCatch({
    rubella_data <- as.data.frame(mo_baseline$moRaw)
    time_column <- rubella_data[, 1]
    
    compartments <- list(
      M = rubella_data[, seq(2, ncol(rubella_data), by = 9)],
      S = rubella_data[, seq(3, ncol(rubella_data), by = 9)],
      E = rubella_data[, seq(4, ncol(rubella_data), by = 9)],
      I = rubella_data[, seq(5, ncol(rubella_data), by = 9)],
      R = rubella_data[, seq(6, ncol(rubella_data), by = 9)],
      V1p = rubella_data[, seq(7, ncol(rubella_data), by = 9)],
      V1np = rubella_data[, seq(8, ncol(rubella_data), by = 9)],
      V2p = rubella_data[, seq(9, ncol(rubella_data), by = 9)],
      V2np = rubella_data[, seq(10, ncol(rubella_data), by = 9)]
    )
    
    list(time_column = time_column, compartments = compartments)
  }, error = function(e) {
    warning("Error in process_raw_model_data: ", e$message)
    NULL
  })
}

# Function to create plot data for each compartment
create_plot_data <- function(time_column, compartment_data, compartment_name) {
  data.frame(time = time_column, 
             total = rowSums(compartment_data), 
             compartment = compartment_name)
}

# Function to plot a single compartment
plot_compartment <- function(data, title) {
  ggplot(data, aes(x = time, y = total)) +
    geom_line() +
    geom_point() +
    labs(title = title, x = "Time", y = "Count") +
    theme_minimal()
}

plot_all_compartments <- function(plot_data_list) {
  tryCatch({
    plots <- lapply(names(plot_data_list), function(name) {
      plot_compartment(plot_data_list[[name]], paste("Total", name, "Compartment Over Time"))
    })
    arranged_plots <- do.call(gridExtra::arrangeGrob, c(plots, ncol = 3))  # Store the grid of plots as a grob
    return(arranged_plots)  # Return the arranged grob instead of plotting it
  }, error = function(e) {
    warning("Error in plot_all_compartments: ", e$message)
    NULL
  })
}

# Function to plot total population
plot_total_population <- function(model_data, true_data) {
  tryCatch({
    ggplot() +
      geom_line(data = model_data, aes(x = time, y = total_population, color = "Model"), size = 1) +
      geom_line(data = true_data, aes(x = Year, y = Population, color = "True"), size = 1) +
      labs(title = "Total Population Over Time",
           x = "Year",
           y = "Population",
           color = "Data Source") +
      theme_minimal() +
      scale_color_manual(values = c("Model" = "blue", "True" = "red"))
  }, error = function(e) {
    warning("Error in plot_total_population: ", e$message)
    NULL
  })
}

# Function to generate seroprevalence plot
generate_seroprevalence_plot <- function(yr, true_data, mop, rubella_data) {
  tryCatch({
    # Implementation of seroprevalence plot
    # This is a placeholder and should be replaced with actual implementation
    ggplot() +
      geom_point(aes(x = 1:length(true_data), y = true_data)) +
      labs(title = "Seroprevalence Plot", x = "Age Group", y = "Seroprevalence")
  }, error = function(e) {
    warning("Error in generate_seroprevalence_plot: ", e$message)
    NULL
  })
}

# Main function to run all plots
run_all_plots <- function(mo_baseline, true_deaths_file, true_population_file) {
  plots <- list()
  
  mop <- read_model_output(mo_baseline)
  plots$death_plot <- plot_death_compartments(mop)
  
  true_deaths <- read_true_deaths(true_deaths_file)
  
  raw_data <- process_raw_model_data(mo_baseline)
  if (!is.null(raw_data)) {
    plot_data_list <- lapply(names(raw_data$compartments), function(name) {
      create_plot_data(raw_data$time_column, raw_data$compartments[[name]], name)
    })
    names(plot_data_list) <- names(raw_data$compartments)
    
    plots$compartment_plots <- plot_all_compartments(plot_data_list)
    
    total_population_model <- rowSums(sapply(raw_data$compartments, rowSums))
    plot_data_total <- data.frame(time = raw_data$time_column, total_population = total_population_model)
    
    true_population_data <- read_excel(true_population_file)
    true_population_data <- true_population_data[-1, ]  # Remove the first empty row
    true_population_data <- true_population_data %>% mutate(across(everything(), as.numeric))  # Convert columns to numeric
    total_true_population <- colSums(true_population_data)  # Sum the true population across all age groups for each year
    true_population_df <- data.frame(Year = as.numeric(names(total_true_population)), Population = total_true_population)
    
    plots$total_population_plot <- plot_total_population(plot_data_total, true_population_df)
    
    true_2018 <- c(32.7, 66.7, 86.2, 92.5, 92.8, 93.8, 93.1, 90.3, 87.7, 90.7, 87.8)
    plots$seroprevalence_plot <- generate_seroprevalence_plot(2019, true_2018, mop, raw_data)
  }
  
  return(plots)
}


# Run all plots
plots <- run_all_plots(mo_baseline, "data/mortality_rates.xlsx", "data/TrueSApopulation.xlsx")
grid::grid.newpage() 
grid::grid.draw(plots$compartment_plots)
# Display plots

