# Packages needed
library(ggplot2)
library(dplyr)
library(gridExtra)

#### Plots ####

# Read in processed data
mop = mo_baseline$moPostprocessing[[1]] |>
  mutate(age_group=as_factor(age_group))

# Esquisser Check
mop%>%esquisse::esquisser()

### Checking Immune compartments:
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
true_deaths = read_excel("/Users/taahir/Downloads/mortality_rates.xlsx", sheet = "totdeaths")
true_deaths = true_deaths[,-1]

# Read in Raw Model Data
rubella_data  = as.data.frame(mo_baseline$moRaw)

# Extract the 'time' column 
time_column <- rubella_data[, 1]

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


######################## total population plot#####3


# Sum all compartments across all ages for each time point
total_population <- rowSums(M_columns) + rowSums(S_columns) + rowSums(I_columns) + rowSums(R_columns) + rowSums(V_columns)

# Create a data frame for plotting
plot_data_total <- data.frame(time = time_column, total_population = total_population)

true_population_path <- '/Users/taahir/Downloads/TrueSApopulation.xlsx'
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
modeled_population_df <- data.frame(time = time_column, total_population = total_population)

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

###### BIRTHS PLOT######
modeled_births <- c(
  969828, 963390, 954574, 984308, 1031997, 1074006, 1111171, 1131070, 1217792, 
  1158903, 1152671, 1167985, 1187329, 1194069, 1204040, 1183143, 1151026, 
  1169583, 1212008, 1235776, 1194619, 1176955, 1176955, 1176955, 1176955, 
  1176955, 1176955, 1176955, 1176955, 1176955, 1176955
)

# Prepare a data frame for modeled births
modeled_births_df <- data.frame(
  year = 2000:2030,  # Assuming the years are from 1990 to 2020
  value = modeled_births,
  source = "Modeled Births"
)

# Filter and prepare the Births data from mop
Births <- mop %>% 
  filter(variable == 'births_D', value > 0) %>%
  mutate(source = "mop Births") %>%
  select(year, value, source)

# Combine both datasets
combined_births <- bind_rows(modeled_births_df, Births)

# Plot both Modeled Births(data) vs mop Births(Model)
ggplot(combined_births, aes(x = year, y = value, color = source, group = source)) +
  geom_line() +
  geom_point() +
  labs(title = "Modeled Births vs. mop Births Over Time",
       x = "Year", y = "Births") +
  theme_minimal() +
  scale_color_manual(values = c("Modeled Births" = "blue", "mop Births" = "red"))



######DEATH PLOTS######
Deaths <- mop %>% 
  filter(variable == 'deaths_D',) %>%
  mutate(source = "mop Deaths") %>%
  select(year, value, source, age_group)



# Define the original age groups
age_groups <- c(
  "0-1 month", "1-2 month", "2-3 month", "3-4 month", "4-5 month", "5-6 month", "6-7 month",
  "7-8 month", "8-9 month", "9-10 month", "10-11 month", "11-12 month", "12-13 month", "13-14 month",
  "14-15 month", "15-16 month", "16-17 month", "17-18 month", "18-19 month", "19-20 month", "20-21 month",
  "21-22 month", "22-23 month", "23-24 month", "24-25 month", "25-26 month", "26-27 month", "27-28 month",
  "28-29 month", "29-30 month", "30-31 month", "31-32 month", "32-33 month", "33-34 month", "34-35 month",
  "35-36 month", "36-37 month", "37-38 month", "38-39 month", "39-40 month", "40-41 month", "41-42 month",
  "42-43 month", "43-44 month", "44-45 month", "45-46 month", "46-47 month", "47-48 months", "4-5 years",
  "5-6 years", "6-7 years", "7-8 years", "8-9 years", "9-10 years", "10-11 years", "11-12 years",
  "12-13 years", "13-14 years", "14-15 years", "15-16 years", "16-17 years", "17-18 years", "18-19 years",
  "19-20 years", "20-21 years", "21-22 years", "22-23 years", "23-24 years", "24-25 years", "25-26 years",
  "26-27 years", "27-28 years", "28-29 years", "29-30 years", "30-31 years", "31-32 years", "32-33 years",
  "33-34 years", "34-35 years", "35-36 years", "36-37 years", "37-38 years", "38-39 years", "39-40 years",
  "40-41 years", "41-42 years", "42-43 years", "43-44 years", "44-45 years", "45-46 years", "46-47 years",
  "47-48 years", "48-49 years", "49-50 years", "50-51 years", "51-52 years", "52-53 years", "53-54 years",
  "54-55 years", "55-56 years", "56-57 years", "57-58 years", "58-59 years", "59-60 years", "60-70 years",
  "70-80 years", "80-90 years", "90+"
)



model_deaths = Deaths %>% filter(age_group =="All") %>% select(c(year, value))
colnames(model_deaths) = c('year', 'model_deaths')
true_deaths <- c(
  499120.6367, 529547.5881, 567709.074, 607862.7901, 642885.2066, 652493.6223, 643787.7158, 
  622303.1151, 598679.17, 576839.0826, 552450.1591, 528737.076, 515257.216, 506029.9541, 
  501666.2887, 503039.8281, 506530.0318, 511686.0082, 513934.7023, 515655.518, 517576.822, 
  520220.1477, 523254.2808, 526794.1637, 531096.2981, 536087.3945, 541694.5845, 547904.0449, 
  554633.2026, 561906.1909, 569705.2916
)
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

# Manually map these into larger age groups
mapped_age_groups <- c(
  rep("0-1 year", 12),
  rep("1-2 years", 12),
  rep("2-3 years", 12),
  rep("3-4 years", 12),
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
  rep("0-14", 58),
  rep("15-19", 5),
  rep("20-24", 5),
  rep("25-29", 5),
  rep("30-34", 5),
  rep("35-39", 5),
  rep("40-44", 5),
  rep("45-49", 5),
  rep("50-54", 5),
  rep("55-60", 6),
  rep("60+", 4)
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


# Read the Excel file
mortality_data <- read_excel("/Users/taahir/Downloads/mortality_rates.xlsx")

# Inspect the sheet names to find relevant data
sheet_names <- excel_sheets("/Users/taahir/Downloads/mortality_rates.xlsx")
print(sheet_names)

# Read the sheets to understand their structure
true_deaths <- read_excel("/Users/taahir/Downloads/mortality_rates.xlsx", sheet = "totdeaths")
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
       geom_line(aes(y = total_value, color = "Model Deaths"), size = 1) +
       geom_line(aes(y = true_deaths, color = "True Deaths"), size = 1) +
       labs(title = paste("Deaths in Age Group:", unique(.$fat_age)),
            x = "Year",
            y = "Number of Deaths",
            color = "Legend") +
       theme_minimal())

# Print the plots
for (p in plot_list$plot) {
  print(p)
}





