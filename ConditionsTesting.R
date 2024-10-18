initialize_population <- function(
    age_groups,             # Vector of age group names
    total_population,       # Named vector of total population per age group
    immunity_data,          # Data frame with Positive and Negative percentages
    r_split_by_age,         # Vector with %R for each age band, repeated as needed
    e_i_split = c(E = 0.5, I = 0.5)  # Split of remaining positives between E and I
) {
  # Initialize an empty data frame for the results in wide format
  initial_conditions <- data.frame(
    AgeGroup = age_groups,
    S = 0, E = 0, I = 0, R = 0, V1p = 0, V1np = 0, V2p = 0, V2np = 0, M = 0
  )
  
  # Loop over each age group to allocate the population
  for (i in seq_along(age_groups)) {
    age <- age_groups[i]
    pop_age_group <- total_population[age]  # Total population for this age group
    
    # Get the positive and negative immunity percentages
    pos_percentage <- immunity_data$Positive_2018[i] / 100
    neg_percentage <- immunity_data$Negative_2018[i] / 100
    
    # Calculate the positive and negative populations
    pos_pop <- pop_age_group * pos_percentage
    neg_pop <- pop_age_group * neg_percentage
    
    # Calculate the number of positives in R based on r_split_by_age
    r_proportion <- r_split_by_age[i] / 100
    r_pop <- pos_pop * r_proportion
    
    # Split the remaining positives between E and I
    remaining_pop <- pos_pop - r_pop
    ei_distribution <- remaining_pop * e_i_split
    
    # Assign the populations to the appropriate compartments
    initial_conditions[i, "S"] <- neg_pop  # All negatives go to S
    initial_conditions[i, "R"] <- r_pop    # Recovered
    initial_conditions[i, "E"] <- ei_distribution["E"]  # Exposed
    initial_conditions[i, "I"] <- ei_distribution["I"]  # Infected
  }
  
  return(initial_conditions)
}

# Define the age groups 
age_groups <- c(
  "0-6 month", "6-12 month", "1-2 years", "2-3 years", "3-4 years", "4-5 years",
  "5-6 years", "6-7 years", "7-8 years", "8-9 years", "9-10 years", "10-11 years",
  "11-12 years", "12-13 years", "13-14 years", "14-15 years", "15-16 years", 
  "16-17 years", "17-18 years", "18-19 years", "19-20 years", "20-21 years", 
  "21-22 years", "22-23 years", "23-24 years", "24-25 years", "25-26 years", 
  "26-27 years", "27-28 years", "28-29 years", "29-30 years", "30-31 years", 
  "31-32 years", "32-33 years", "33-34 years", "34-35 years", "35-36 years", 
  "36-37 years", "37-38 years", "38-39 years", "39-40 years", "40-41 years", 
  "41-42 years", "42-43 years", "43-44 years", "44-45 years", "45-46 years", 
  "46-47 years", "47-48 years", "48-49 years", "49-50 years", "50-51 years", 
  "51-52 years", "52-53 years", "53-54 years", "54-55 years", "55-56 years", 
  "56-57 years", "57-58 years", "58-59 years", "59-60 years", "60-70 years", 
  "70+"
)

# Define population sizes for each age group
total_population <- c(
  516155.50, 501322, 1002644, 974306, 941896, 905484, 871092, 842889, 
  824824, 814255, 806694, 796130, 780251, 756936, 729134, 700655, 676397, 
  658843, 650029, 646960, 645182, 640917, 633662, 621658, 605824, 588939, 
  572093, 553825, 534378, 514265, 493722, 473335, 453735, 435278, 417670, 
  400846, 384040, 366496, 347870, 329008, 309743, 292361, 279882, 273937, 
  272787, 274085, 274434, 271924, 264564, 253582, 241586, 230870, 221780, 
  215210, 210518, 206822, 202403, 196094, 186720, 174997, 161688, 1315126, 
  550746
)
names(total_population) <- age_groups

# Immunity data 
immunity_data <- data.frame(
  Positive_2018 = rep(c(23.5, 59.8, 82.7, 91.4, 92.6, 92.9, 92.3, 91, 90.5, 89.2), each = 6),
  Negative_2018 = 100 - rep(c(23.5, 59.8, 82.7, 91.4, 92.6, 92.9, 92.3, 91, 90.5, 89.2), each = 6)
)

# Define the R split 
r_split_by_age <- rep(c(50, 75, 90, 95), times = c(6, 5, 5, 47))

# Generate the initial conditions
initial_conditions <- initialize_population(
  age_groups, total_population, immunity_data, r_split_by_age
)
initial_conditions_new =initial_conditions %>% select(c('M','S', 'E', 'I','R','V1p', 'V1np','V2p','V2np')) 

###### DATELESS  MODELLING ############################################################################################################################################################################################################################################################################################################################################################
# Define the years to add (1950 to 1984)
source(here::here("R/utils.R"))
LOG <- makeLogger()
source(here::here("R/model_setup.R"))
source(here::here("R/rubellaModel.R"))
param_Baseline <- getModelInputs(scenario="Rubella")

years_to_add <- 1950:1984

# 1. Get the first column or value to repeat for s, v1, and v2 matrices
first_col_s <- param_Baseline$s[, 1, drop = FALSE]
first_col_v1 <- param_Baseline$v1[, 1, drop = FALSE]
first_col_v2 <- param_Baseline$v2[, 1, drop = FALSE]

# Create new matrices by repeating the first column for the new years
new_s <- matrix(rep(first_col_s, length(years_to_add)), 
                nrow = nrow(param_Baseline$s), 
                ncol = length(years_to_add))
new_v1 <- matrix(rep(first_col_v1, length(years_to_add)), 
                 nrow = nrow(param_Baseline$v1), 
                 ncol = length(years_to_add))
new_v2 <- matrix(rep(first_col_v2, length(years_to_add)), 
                 nrow = nrow(param_Baseline$v2), 
                 ncol = length(years_to_add))

# Set column names for the new years
colnames(new_s) <- as.character(years_to_add)
colnames(new_v1) <- as.character(years_to_add)
colnames(new_v2) <- as.character(years_to_add)

# Prepend the new columns to the original matrices
param_Baseline$s <- cbind(new_s, param_Baseline$s)
param_Baseline$v1 <- cbind(new_v1, param_Baseline$v1)
param_Baseline$v2 <- cbind(new_v2, param_Baseline$v2)

# 2. Extend other parameters (births, eff_1_D, eff_2_D, fert_rate)

# Generate new values for the years 1950 to 1984
new_percfem <- rep(param_Baseline$perc_fem[1], length(years_to_add))

new_births <- rep(param_Baseline$births[1], length(years_to_add))
new_eff_1_D <- rep(param_Baseline$eff_1_D[1], length(years_to_add))
new_eff_2_D <- rep(param_Baseline$eff_2_D[1], length(years_to_add))
new_fert_rate <- rep(param_Baseline$fert_rate[1], length(years_to_add))

# Prepend the new values to the existing lists
param_Baseline$perc_fem = c(
  setNames(new_percfem, years_to_add),
  param_Baseline$perc_fem
)
param_Baseline$births <- c(
  setNames(new_births, years_to_add),
  param_Baseline$births
)

param_Baseline$eff_1_D <- c(
  setNames(new_eff_1_D, years_to_add),
  param_Baseline$eff_1_D
)

param_Baseline$eff_2_D <- c(
  setNames(new_eff_2_D, years_to_add),
  param_Baseline$eff_2_D
)

param_Baseline$fert_rate <- c(
  setNames(new_fert_rate, years_to_add),
  param_Baseline$fert_rate
)

# 3. Verify the structure of the updated parameters
print(names(param_Baseline$births))
print(colnames(param_Baseline$s))
print(dim(param_Baseline$v1))
###### RUN #############

startyear = 1950
# R0 from R script
#Initial Values functionalise
#Data free model: age strucutre, scaled death and birth rate(can fix over time)

#### Run the model ####
setLogLevel(LEVEL$TRACE)
# setLogLevel(LEVEL$INFO) #to shush the logging


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



