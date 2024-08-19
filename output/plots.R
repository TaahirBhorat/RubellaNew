# Packages needed
# MAKE TABLE OF CONTENTS AND ORDER
library(ggplot2)
library(dplyr)
library(gridExtra)

#### Plots ####

# Read in processed data
mop = mo_baseline$moPostprocessing[[1]] |>
  mutate(age_group=as_factor(age_group))

############# Esquisser for looking at processed data and variables ###########################################
#mop%>%esquisse::esquisser()

############  Death Compartments Plots (age stratified)#####################################################################
mop%>%
  filter(variable %in% "deaths_D") %>%
  ggplot() +
  aes(
    x = year,
    y = value,
    colour = age_group,
    group = age_group
  ) +
  geom_line() +
  scale_color_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(variable), scales = "free_y")

mop_death = mop%>%
  filter(variable %in% "deaths_D")


# read in true Death data
true_deaths = read_excel("data/mortality_rates.xlsx", sheet = "totdeaths")
true_deaths = true_deaths[,-1]


####### Overall Compartment plots over time ########################################################
# Read in Raw Model Data
rubella_data  = as.data.frame(mo_baseline$moRaw)

# Extract the 'time' column 
time_column <- rubella_data[, 1]

# Getting the compartments from the raw data
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

# Create compartment data frames for plotting
plot_data_M <- data.frame(time = time_column, total = total_M, compartment = "M")
plot_data_S <- data.frame(time = time_column, total = total_S, compartment = "S")
plot_data_I <- data.frame(time = time_column, total = total_I, compartment = "I")
plot_data_R <- data.frame(time = time_column, total = total_R, compartment = "R")
plot_data_V <- data.frame(time = time_column, total = total_V, compartment = "V")

# Combine compartment data frames into one
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


######################## total population plot##########################################################################


# Sum all compartments across all ages for each time point
total_population_model <- rowSums(M_columns) + rowSums(S_columns) + rowSums(I_columns) + rowSums(R_columns) + rowSums(V_columns)

# Create a data frame for plotting
plot_data_total <- data.frame(time = time_column, total_population = total_population_model)

true_population_path <- 'data/TrueSApopulation.xlsx'
true_population_data <- read_excel(true_population_path)

# Remove the first empty row
true_population_data <- true_population_data[-1, ]

# Convert columns to numeric
true_population_data <- true_population_data %>% mutate(across(everything(), as.numeric))

# Sum the true population across all age groups for each year
total_true_population <- colSums(true_population_data)

# Extract the years from the column names
years <- as.numeric(names(total_true_population))

# Create a data frame for the true population
true_population_df <- data.frame(time = years, total_population = total_true_population)

# Create a data frame for the modeled population (already computed)
modeled_population_df <- data.frame(time = time_column, total_population = total_population_model)

# Plot the total population over time, comparing modeled vs. true population
ggplot() +
  geom_line(data = modeled_population_df, aes(x = time, y = total_population, color = "Modeled Population")) +
  geom_point(data = modeled_population_df, aes(x = time, y = total_population, color = "Modeled Population")) +
  geom_line(data = true_population_df, aes(x = time, y = total_population, color = "True Population")) +
  geom_point(data = true_population_df, aes(x = time, y = total_population, color = "True Population")) +
  labs(title = "Total Population Over Time: Modeled vs. True Population",
       x = "Time", y = "Total Population") +
  scale_color_manual(name = "Population Type", values = c("Modeled Population" = "blue", "True Population" = "red")) +
  theme_minimal()

###### Total Births  over time############################################################################################################
true_births <- c(
  969828, 963390, 954574, 984308, 1031997, 1074006, 1111171, 1131070, 1217792, 
  1158903, 1152671, 1167985, 1187329, 1194069, 1204040, 1183143, 1151026, 
  1169583, 1212008, 1235776, 1194619, 1176955, 1176955, 1176955, 1176955, 
  1176955, 1176955, 1176955, 1176955, 1176955, 1176955
)

# Prepare a data frame for modeled births
true_births_df <- data.frame(
  year = 2000:2030,  
  value = true_births,
  source = "True Births"
)

# Filter and prepare the Births data from mop
Births <- mop %>% 
  filter(variable == 'births_D', value > 0) %>%
  mutate(source = "Model Births") %>%
  select(year, value, source)

# Combine both datasets
combined_births <- bind_rows(true_births_df, Births)

# Plot both Modeled Births(data) vs mop Births(Model)
ggplot(combined_births, aes(x = year, y = value, color = source, group = source)) +
  geom_line() +
  geom_point() +
  labs(title = "True Births vs. Model Births Over Time",
       x = "Year", y = "Births") +
  theme_minimal() +
  scale_color_manual(values = c("True Births" = "blue", "Model Births" = "red"))



######DEATH PLOTS##############################################################################
Deaths <- mop %>% 
  filter(variable == 'deaths_D') %>%
  mutate(source = "mop Deaths") %>%
  select(year, value, source, age_group)



# Define the original age groups
age_groups <- c(
  "0-6 month", "6-12 month","1-2 years","2-3 years","3-4 years", "4-5 years",
  "5-6 years", "6-7 years", "7-8 years", "8-9 years", "9-10 years", "10-11 years", "11-12 years",
  "12-13 years", "13-14 years", "14-15 years", "15-16 years", "16-17 years", "17-18 years", "18-19 years",
  "19-20 years", "20-21 years", "21-22 years", "22-23 years", "23-24 years", "24-25 years", "25-26 years",
  "26-27 years", "27-28 years", "28-29 years", "29-30 years", "30-31 years", "31-32 years", "32-33 years",
  "33-34 years", "34-35 years", "35-36 years", "36-37 years", "37-38 years", "38-39 years", "39-40 years",
  "40-41 years", "41-42 years", "42-43 years", "43-44 years", "44-45 years", "45-46 years", "46-47 years",
  "47-48 years", "48-49 years", "49-50 years", "50-51 years", "51-52 years", "52-53 years", "53-54 years",
  "54-55 years", "55-56 years", "56-57 years", "57-58 years", "58-59 years", "59-60 years", "60-70 years",
  "70+"
)


#### Overall true vs Model Deaths######################################################################################################
model_deaths = Deaths %>% filter(age_group =="All") %>% select(c(year, value))
colnames(model_deaths) = c('year', 'model_deaths')
true_deaths <- c(
  499120.6367, 529547.5881, 567709.074, 607862.7901, 642885.2066, 652493.6223, 643787.7158, 
  622303.1151, 598679.17, 576839.0826, 552450.1591, 528737.076, 515257.216, 506029.9541, 
  501666.2887, 503039.8281, 506530.0318, 511686.0082, 513934.7023, 515655.518, 517576.822, 
  520220.1477, 523254.2808, 526794.1637, 531096.2981, 536087.3945, 541694.5845, 547904.0449, 
  554633.2026, 561906.1909, 569705.2916
) 
# UN Deaths
#true_deaths = c(490138,526754,574030,622283,641921,659846,669557,663057,648296,624186,594055,568156,
#                545852,544728,536322,522822,519287,508901,515170,521415,586416,733898,586473,584018,
#                591227,600458,609623,618696,627927,637394,646547)
model_deaths$true_deaths = true_deaths
# Plot the data using ggplot2
ggplot(model_deaths, aes(x = year)) +
  geom_line(aes(y = model_deaths, color = "Model Deaths"), size = 1) +
  geom_line(aes(y = true_deaths, color = "True Deaths"), size = 1) +
  labs(title = "Total deaths: Model vs True Over Time",
       x = "Year",
       y = "Number of Deaths",
       color = "Legend") +
  theme_minimal()
######## Fat Age Group Death: Model vs True###################################################################################
# Manually map these into larger age groups
mapped_age_groups <- c(
  rep("0-1 year", 2),
  rep("1-2 years", 1),
  rep("2-3 years", 1),
  rep("3-4 years", 1),
  rep("4-5 years", 1),
  rep("5-10 years", 5),
  rep("10-20 years", 10),
  rep("20-30 years", 10),
  rep("30-40 years", 10),
  rep("40-50 years", 10),
  rep("50-60 years", 10),
  rep("60-70 years", 1),
  rep("70-80 years", 1),
  rep("80-90 years", 1),
  rep("90+ years", 1)
)

mapped_age_groups <- c(
  rep("0-14", 16),
  rep("15-19", 5),
  rep("20-24", 5),
  rep("25-29", 5),
  rep("30-34", 5),
  rep("35-39", 5),
  rep("40-44", 5),
  rep("45-49", 5),
  rep("50-54", 5),
  rep("55-60", 5),
  rep("60+", 2)
)


# Create a data frame to show the original and mapped age groups
result <- data.frame(
  Original_Age_Groups = age_groups,
  Mapped_Age_Groups = mapped_age_groups
)

print(result)


model_deaths_age = Deaths %>% filter(age_group !="All")
model_deaths_age$fat_age = rep(mapped_age_groups,31)
summarized_data <- model_deaths_age %>%
  group_by(year, fat_age) %>%
  summarize(total_value = sum(value))


# Read the true deaths
true_deaths <- read_excel("data/mortality_rates.xlsx", sheet = "totdeaths")
print(head(true_deaths))


# Extract the relevant columns from the true deaths data
true_deaths_long <- true_deaths %>%
  pivot_longer(cols = -`Age band`, names_to = "year", values_to = "true_deaths") %>%
  mutate(year = as.integer(year))

# Merge the model deaths data with the true deaths data
merged_data <- summarized_data %>%
  left_join(true_deaths_long, by = c("year", "fat_age" = "Age band"))

# Create the plots using ggplot2
plot_list <- merged_data %>%
  group_by(fat_age) %>%
  do(plot = ggplot(data = ., aes(x = year)) +
       geom_point(aes(y = total_value, color = "Model Deaths"), size = 1) +
       geom_point(aes(y = true_deaths, color = "True Deaths"), size = 1) +
       labs(title = paste("Deaths in Age Group:", unique(.$fat_age)),
            x = "Year",
            y = "Number of Deaths",
            color = "Legend") +
       theme_minimal())

# Print the plots
for (p in plot_list$plot) {
  print(p)
}
################## Sero-Prevalence Plots ########################################################################################################################################################################
# Check what the assay actually picks up
# Add M (Research: see if its detected by sero_prec instrument) 
generate_seroprevalence_plot <- function(yr, true_positive_percentage) {
  mop = mo_baseline$moPostprocessing[[1]] |>
    mutate(age_group=as_factor(age_group))
  rubella_data  = as.data.frame(mo_baseline$moRaw)
  
  # Extract the 'time' column 
  time_column <- rubella_data[, 1]
  
  # Getting the compartments from the raw data
  M_columns <- rubella_data[, seq(2, ncol(rubella_data), by=5)]
  S_columns <- rubella_data[, seq(3, ncol(rubella_data), by=5)]
  I_columns <- rubella_data[, seq(4, ncol(rubella_data), by=5)]
  R_columns <- rubella_data[, seq(5, ncol(rubella_data), by=5)]
  V_columns <- rubella_data[, seq(6, ncol(rubella_data), by=5)]
  
  # Ensure column names are consistent
  colnames(I_columns) <- paste0("Column", 1:ncol(I_columns))
  colnames(R_columns) <- paste0("Column", 1:ncol(R_columns))
  colnames(M_columns) <- paste0("Column", 1:ncol(M_columns))
  colnames(S_columns) <- paste0("Column", 1:ncol(S_columns))
  colnames(V_columns) <- paste0("Column", 1:ncol(V_columns))
  
  groups <- list(
    group1 = 1:6,
    group2 = 7:10,
    group3 = 11:15,
    group4 = 16:20,
    group5 = 21:25,
    group6 = 26:30,
    group7 = 31:35,
    group8 = 36:40,
    group9  = 41:45,
    group10 = 46:50,
    group11 = 51:63
  )
  
  RecVac_D = mop %>% filter(variable == 'RecVac_D' & age_group != 'All') %>% select(c(age_group, value,year))
  RecVac_D_spread <- RecVac_D %>%
    pivot_wider(names_from = age_group, values_from = value)
  RecVac_D_spread <- RecVac_D_spread %>% select(-year)
  
  # Function to sum correct columns to new age groups
  sum_groups <- function(df, groups) {
    col_sums_list <- list()
    for (i in 1:length(groups)) {
      group_name <- paste0("sum_group", i)
      col_sums_list[[group_name]] <- rowSums(df[, groups[[i]]])
    }
    new_df <- as.data.frame(col_sums_list)
    return(new_df)
  }
  
  M_new = sum_groups(M_columns, groups)
  S_new = sum_groups(S_columns, groups)
  I_new = sum_groups(I_columns, groups)
  R_new = sum_groups(R_columns, groups)
  V_new = sum_groups(V_columns, groups)
  RecVac_D_spread_new = sum_groups(RecVac_D_spread, groups)
  
  # Set the row names of the DataFrame to the dates
  M_new$date <- time_column
  S_new$date <- time_column
  I_new$date <- time_column
  R_new$date <- time_column
  V_new$date <- time_column
  RecVac_D_spread_new$date <- seq(2000, 2030, 1)
  
  M_2018 = M_new %>% filter(date == yr) %>% select(-date)
  S_2018 = S_new %>% filter(date == yr) %>% select(-date)
  I_2018 = I_new %>% filter(date == yr) %>% select(-date)
  R_2018 = R_new %>% filter(date == yr) %>% select(-date)
  V_2018 = V_new %>% filter(date == yr) %>% select(-date)
  
  # Function to sum the columns up to a specified date
  sum_up_to_date <- function(data, target_date) {
    subset_data <- subset(data, date <= target_date)
    sums <- colSums(subset_data[, !colnames(subset_data) %in% c("date")])
    return(sums)
  }
  
  RecVac_D_spread_new_2018 <- data.frame(t(sum_up_to_date(RecVac_D_spread_new, yr)))
  
  sero_pos_2018 = data.frame(sero_pos = colSums(rbind(I_2018, R_2018, RecVac_D_spread_new_2018)), 
                             Total_Pop = colSums(rbind(M_2018, S_2018, I_2018, R_2018, V_2018)))
  colnames(sero_pos_2018) = c("sero_pos", 'Total Pop')
  
  sero_pos_2018$seroprevalence_model <- sero_pos_2018$sero_pos / sero_pos_2018$`Total Pop` * 100
  sero_pos_2018$seroprevalence_true <- true_positive_percentage
  
  age_groups <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-45", "45-49", "50+")
  sero_pos_2018$age_group <- factor(age_groups, levels = age_groups)
  
  sero_pos_2018_long <- sero_pos_2018 %>%
    select(age_group, seroprevalence_model, seroprevalence_true) %>%
    pivot_longer(cols = c("seroprevalence_model", "seroprevalence_true"), 
                 names_to = "type", 
                 values_to = "seroprevalence")
  
  # Plotting
  ggplot(sero_pos_2018_long, aes(x = age_group, y = seroprevalence, fill = type)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("True vs Model Seroprevalence per Age Group in", yr, "vs 2018"), x = "Age Group", y = "Seroprevalence (%)") +
    scale_fill_manual(name = "Seroprevalence", values = c("seroprevalence_model" = "skyblue4", "seroprevalence_true" = "salmon"), 
                      labels = c("Model", "True")) +
    theme_minimal()
}
true_2018  = c(32.7, 66.7, 86.2, 92.5, 92.8, 93.8, 93.1, 90.3, 87.7, 90.7, 87.8)
generate_seroprevalence_plot(2018, true_2018)
################################ CRS COUNTING ##############################################################
# raw_output_df = rubella_data
# head(raw_output_df)
# 
# # Assuming you have a yearly births vector
# 
# 
# 
# # Parametersx
# 
# 
# c <- 0.05  # constant first 16 week pregnancy infection
# b <- unname(param_Baseline$births)  
# ptrans <- param_Baseline$ptrans  #ptrans
# contact_matrix <-param_Baseline$contact  # Example contact matrix, replace with actual data
# yearly_births <- b  
# 
# 
# # Distribute the yearly births across the time vector
# daily_births <- rep(yearly_births / 365.25, each = 365.25)
# daily_births <- daily_births[1:length(time)]  # Ensure it matches the length of the time vector
# b = daily_births
# # Extract data
# time <- raw_output_df[[1]]
# S <- S_columns
# I <- I_columns
# total_pop <- modeled_population_df['total_population']
# 
# # Age indices for childbearing age groups (15-49 years)
# childbearing_age_indices <- 60:93
# 
# # Calculate CRS cases over time
# CRS_cases_over_time <- numeric(length(time))
# 
# for (ti in 1:length(time)) {
#   ti  = 1
#   # Calculate force of infection (Phi_t) for the current time step
#   infectious_D <- I[ti, ] / total_pop[ti, ]
#   lambda_t <- ptrans * as.vector(contact_matrix %*% t(infectious_D))
#   
#   # Adjust f_a by birth rate for childbearing age group
#   f_a <- b[ti]* (total_pop[ti, ] / sum(total_pop[ti, ]))
#   
#   # Extract compartment values for childbearing age group
#   S_childbearing <- I[ti, childbearing_age_indices]
#   
#   # Calculate CRS cases at the current time step
#   CRS_cases <- sum(f_a * c * lambda_t[childbearing_age_indices] * S_childbearing)
#   
#   # Store CRS cases
#   CRS_cases_over_time[ti] <- CRS_cases
# }
# 
# # Create a DataFrame with time and CRS cases
# CRS_cases_df <- data.frame(time = time, CRS_cases = CRS_cases_over_time)
# ggplot(CRS_cases_df, aes(x = time, y = CRS_cases)) +
#   geom_line(color = "blue") +
#   labs(title = "CRS Cases Over Time",
#        x = "Time (days)",
#        y = "CRS Cases") +
#   theme_minimal()
# 
# head(CRS_cases_df)



# Fit u_young and u_old, to fat age group deaths
### Try to leigh one-year age groups. 
## how many people to get correct starting age group


