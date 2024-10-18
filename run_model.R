source(here::here("R/utils.R"))
LOG <- makeLogger()
source(here::here("R/model_setup.R"))
source(here::here("R/rubellaModel.R"))
# R0 from R script
  #Initial Values functionalise
    #Data free model: age strucutre, scaled death and birth rate(can fix over time)

#### Run the model ####
setLogLevel(LEVEL$TRACE)
# setLogLevel(LEVEL$INFO) #to shush the logging

param_Baseline <- getModelInputs(scenario="Rubella")

initialConditions <- read_excel("parameters/DataWorkbookRubella.xlsx", sheet = 'init_cond')[-1]
grp <- read_excel("parameters/DataWorkbookRubella.xlsx", sheet = 'age_ref')[-1]

# Rename the column in the grp dataframe to age_group
colnames(grp)[1] <- "age_group"
##RUN MODEL####
tictoc::tic("Running Baseline")
initialConditions = as.matrix(initialConditions)
initialConditions <- as.vector(t(initialConditions))
mo_baseline <- run_model.D(param_Baseline, initialConditions, timesteps)
tictoc::toc()

# Function to process raw model data
process_raw_model_data <- function(mo_baseline) {
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
    theme_minimal()+ylim(0,NA)
}

# Function to plot all compartments
plot_compartments <- function(mo_baseline) {
  # Process raw model data
  raw_data <- process_raw_model_data(mo_baseline)
  
  if (is.null(raw_data)) {
    stop("Error processing raw model data")
  }
  
  # Create plot data for each compartment
  plot_data_list <- lapply(names(raw_data$compartments), function(name) {
    create_plot_data(raw_data$time_column, raw_data$compartments[[name]], name)
  })
  names(plot_data_list) <- names(raw_data$compartments)
  
  # Plot all compartments
  plots <- lapply(names(plot_data_list), function(name) {
    plot_compartment(plot_data_list[[name]], paste("Total", name, "Compartment Over Time"))
  })
  
  # Arrange the plots in a grid
  arranged_plots <- do.call(gridExtra::arrangeGrob, c(plots, ncol = 3))
  
  # Display the plots
  grid::grid.newpage()
  grid::grid.draw(arranged_plots)
}

plot_compartments(mo_baseline)

# plot the value of pop_D over time as a line plot
plot(mo_baseline$moPostprocessing[[1]]$value[mo_baseline$moPostprocessing[[1]]$variable == "pop_D"], type = "l")
