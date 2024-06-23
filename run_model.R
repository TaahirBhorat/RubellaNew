source(here::here("R/utils.R"))
LOG <- makeLogger()
source(here::here("R/model_setup.R"))
source(here::here("R/diphtheriaModel.R"))

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
mo_baseline <- run_model.D(param_Baseline, initialConditions, timesteps)
tictoc::toc()

#### Plots ####
pltPostProc_yr.D(mo_baseline$moPostprocessing[[1]], var = "clin_Inc_D")

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
