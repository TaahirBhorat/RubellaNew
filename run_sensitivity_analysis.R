source(here::here("R/utils.R"))
LOG <- makeLogger()
source(here::here("R/model_setup.R"))
source(here::here("R/diphtheriaModel.R"))

#### Run the model ####
setLogLevel(LEVEL$TRACE)
# setLogLevel(LEVEL$INFO) #to shush the logging

param_Baseline <- getModelInputs(scenario="Baseline")

initialConditions <- makeInitialConditionsCode.D(
  mtMod_D,
  parameters = param_Baseline,
  coverage_table = readxl::read_excel(
    "data/DataWorkbook.xlsx",
    sheet = "coverage_table",
  ) %>% mutate(across(c(DTPCV1, DTPCV2, DTPCV3), as.numeric))
)

#### Run sensitivity analysis ####
tbParameterSets <- tibble(ptrans_D=seq(1,3.5,0.5)) |>
  rowid_to_column('parameterSet') # Could get this using https://masha-app.shinyapps.io/LHSCalibration

parameterSetIds <- tbParameterSets$parameterSet

parameters_baseline <- getModelInputs(scenario="Baseline")

results <- list()
# Note:
# Can parallelize over here with future::future_map or %dopar% 
# but here be dragons - need to ensure the processes have access to functions and data
# add at own risk :)
for (i in parameterSetIds) {  # Alternatively `map(parameterSetIds, ...)` and collect tibbles, pass result to `bind_rows`
  parameters_updated <- parameters_baseline
  newParms <- tbParameterSets |> filter(parameterSet==i) |> select(-parameterSet) |> as.list()
  parameters_updated[names(newParms)] <- newParms
  mo_scenario <- run_model.D(parameters_updated, initialConditions, timesteps)
  # Need to decide what we want to do with each of the model runs.
  # Perhaps calculate some kind of GOF metric?
  # unique(mo_scenario$moPostprocessing[[1]]$variable)
  # Suppose we had some known data:
  tbKnown <- tibble(year=2020:2024, dataValue=c(13, 9, 11, 7, 4))
  # We can get the GoF using NLL like this:
  GoF <- mo_scenario$moPostprocessing[[1]] |>
    filter(variable=="rep_Clin_D", age_group=="All", unit=="Annual",
           year %between% c(2020, 2024)) |>
    rename(modelValue=value) |>
    left_join(tbKnown, by=c("year"="year")) |>
    summarise(NLL = -sum(dpois(dataValue, lambda=modelValue, log=TRUE))) |>
    pull(NLL)
  results[[i]] <- GoF
}

tbParameterSets$GoF <- as.numeric(results)
tbParameterSets
