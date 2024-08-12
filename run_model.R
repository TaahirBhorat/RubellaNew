source(here::here("R/utils.R"))
LOG <- makeLogger()
source(here::here("R/model_setup.R"))
source(here::here("R/rubellaModel.R"))

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




