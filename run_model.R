source(here::here("R/utils.R"))
LOG <- makeLogger()
source(here::here("R/model_setup.R"))
source(here::here("R/rubellaModel.R"))

#### Run the model ####
setLogLevel(LEVEL$TRACE)
# setLogLevel(LEVEL$INFO) #to shush the logging

param_Baseline <- getModelInputs(scenario="Rubella")

initialConditions <- read_excel("DataWorkbookRubella.xlsx", sheet = 'init_cond')[-1]
grp <- read_excel("DataWorkbookRubella.xlsx", sheet = 'age_ref')[-1]

# Rename the column in the grp dataframe to age_group
colnames(grp)[1] <- "age_group"

# Assign the age_group column to the initialConditions dataframe
#initialConditions$age_group <- grp$age_group
tictoc::tic("Running Baseline")
initialConditions = as.matrix(initialConditions)
initialConditions <- as.vector(t(initialConditions))
mo_baseline <- run_model.D(param_Baseline, initialConditions, timesteps)
tictoc::toc()

#### Plots ####
mop = mo_baseline$moPostprocessing[[1]] |>
  mutate(age_group=as_factor(age_group))
mop%>%esquisse::esquisser()
### Checking Immune compartments:



mop |>
  dplyr::filter(age_group!="All", year>=2022, variable=="imune") |>
ggplot() +
  aes(x = year, y = value, colour = age_group, group = age_group) +
  geom_point() +
  scale_color_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(variable), scales = "free_y")
pltPostProc_yr.D(mo_baseline$moPostprocessing[[1]], var = "all_Inc_D")

#### Scenario example ####
param_Scen1 <- getModelInputs(scenario="Scenario")
tictoc::tic("Running Scenario 1")
mo_Scen1 <- run_model.D(param_Scen1, initialConditions, timesteps)
tictoc::toc()
pltPostProc_yr.D(mo_Scen1$moPostprocessing[[1]], var = "clin_Inc_D")

# The scenario sheet sets the covbyr values for 18-19wks to 0.5 from 2024 onwards
# Because of how ageing works, the 20-21wks age group will become protected
# Here we create 2 simple tibbles looking at this population protected in this age group for verification:
tbBaseline <- mo_baseline$moPostprocessing[[1]] |> filter(variable=="protected_prop_D", age_group=="20-21wks", unit=="Annual") |> transmute(year=year,value=value,scenario="Baseline")
tbScen1 <- mo_Scen1$moPostprocessing[[1]] |> filter(variable=="protected_prop_D", age_group=="20-21wks", unit=="Annual") |> transmute(year=year,value=value,scenario="Scenario")
# We can plot this to check:
bind_rows(tbBaseline, tbScen1) |>
  ggplot(aes(x=year, y=value, color=scenario)) +
    geom_point() +
    geom_line(alpha=0.6) +
    scale_color_manual(values=c("black", "red")) +
    theme_minimal() +
    labs(title="Protected proportion of infants protected", x="Year", y="Proportion protected")


rubella_data  = as.data.frame(mo_baseline$moRaw)

library(ggplot2)
library(dplyr)
library(gridExtra)

# Extract the 'time' column (assumed to be the first column in your data)
time_column <- rubella_data[, 1]

# Extract columns for each compartment using their respective column indices
M_columns <- rubella_data[, seq(2, ncol(rubella_data), by=5)]
S_columns <- rubella_data[, seq(3, ncol(rubella_data), by=5)]
I_columns <- rubella_data[, seq(4, ncol(rubella_data), by=5)]
R_columns <- rubella_data[, seq(5, ncol(rubella_data), by=5)]
V_columns <- rubella_data[, seq(6, ncol(rubella_data), by=5)]

# Sum all compartments across all ages for each time point
total_M <- rowSums(M_columns)
total_S <- rowSums(S_columns)
total_I <- rowSums(I_columns)
total_R <- rowSums(R_columns)
total_V <- rowSums(V_columns)

# Create data frames for plotting
plot_data_M <- data.frame(time = time_column, total = total_M, compartment = "M")
plot_data_S <- data.frame(time = time_column, total = total_S, compartment = "S")
plot_data_I <- data.frame(time = time_column, total = total_I, compartment = "I")
plot_data_R <- data.frame(time = time_column, total = total_R, compartment = "R")
plot_data_V <- data.frame(time = time_column, total = total_V, compartment = "V")

# Combine data frames into one
plot_data <- rbind(plot_data_M, plot_data_S, plot_data_I, plot_data_R, plot_data_V)

# Define a plotting function
plot_compartment <- function(data, title) {
  ggplot(data, aes(x = time, y = total)) +
    geom_line() +
    geom_point() +
    labs(title = title, x = "Time", y = "Count") +
    theme_minimal()
}

# Generate individual plots for each compartment
plot_M <- plot_compartment(plot_data_M, "Total M Compartment Over Time")
plot_S <- plot_compartment(plot_data_S, "Total S Compartment Over Time")
plot_I <- plot_compartment(plot_data_I, "Total I Compartment Over Time")
plot_R <- plot_compartment(plot_data_R, "Total R Compartment Over Time")
plot_V <- plot_compartment(plot_data_V, "Total V Compartment Over Time")

# Arrange the plots in a grid
grid.arrange(plot_M, plot_S, plot_I, plot_R, plot_V, ncol = 1)



###### AGE: 


# Extract the 'time' column (assumed to be the first column in your data)
time_column <- rubella_data[, 1]

# Extract the 'I' compartments
I_columns <- rubella_data[, seq(4, ncol(rubella_data), by=5)]

# Define the age bins based on your specified age categories
age_bins <- c(
  seq(0.5/12, 47.5/12, by=1/12), # Monthly bins from 0-47 months (0-4 years)
  seq(4.5, 59.5, by=1),          # Yearly bins from 5-59 years
  seq(65, 95, by=10)             # Decade bins from 60-90+ years
)

# Ensure the length of age_bins matches the number of columns in I_columns
if (length(age_bins) != ncol(I_columns)) {
  stop("Number of age bins does not match the number of I compartment columns.")
}

# Calculate the total infections for each time point
total_I <- rowSums(I_columns)

# Calculate the average age of infection for each time point
average_age_infection <- rowSums(I_columns * age_bins) / total_I

# Create a data frame for plotting
plot_data <- data.frame(time = time_column, average_age = average_age_infection)

# Plot the average age of infection over time
ggplot(plot_data, aes(x = time, y = average_age)) +
  geom_line() +
  labs(title = "Average Age of Rubella Infection Over Time", x = "Time", y = "Average Age of Infection") +
  theme_minimal()







