source(here::here("R/utils.R"))
LOG=makeLogger()
source(here::here("R/model_setup.R"))
source(here::here("R/rubellaModel.R"))
source(here::here("run_model.R"))
source(here::here("output/Plots.R"))
### Add new natal death rates
#### Fit a scalar to deaths for 6-12 months
##### Solve for both at once, use 0-1 age group as target. 


############ TRUE DEATH INPUTS ######################################################################################################################################################################################################################################
  # Young death true output
target_output_young =unlist(as.vector(true_deaths_long %>% filter(`Age band`=='0-14') %>% select(true_deaths))) # Replace with actual target values
  # Old death true output
target_output_old =unlist(as.vector(true_deaths_long %>% filter(`Age band`=='60+') %>% select(true_deaths))) # Replace with actual target values

######### MINIMISING FUNCTION ##################################################################################################################################################################################################################################
  # Objective function to evaluate the difference between model death output and true output(per age class)
objective_function=function(param_value, target_output,age_young) {
  param_Baseline=getModelInputs(scenario="Rubella")
  if(age_young == TRUE){
    # Adjust parameters for young ageing rate
    
    param_Baseline$u[c(1,2)]=rep(param_value, 2)  #
  } else {
    # Adjust parameters for old ageing rate
    param_Baseline$u[c(62,63)]=rep(param_value, 2)  
  }
  run_out=run_model.D(param_Baseline, initialConditions, timesteps)
  out = run_out$moPostprocessing[[1]] |>
    mutate(age_group=as_factor(age_group))
  
  
  Deaths=out %>% 
    filter(variable == 'deaths_D') %>%
    mutate(source = "mop Deaths") %>%
    select(year, value, source, age_group)
  
  result=data.frame(
    Original_Age_Groups = age_groups,
    Mapped_Age_Groups = mapped_age_groups
  )
  model_deaths_age = Deaths %>% filter(age_group !="All")
  model_deaths_age$fat_age = rep(mapped_age_groups,31)
  if(age_young == TRUE){
    model_output=as.vector(unlist(model_deaths_age %>%
                                       group_by(year, fat_age) %>%
                                       summarize(total_value = sum(value)) %>% filter(fat_age == '0-14') %>% select(total_value)))
  } else {
    model_output=as.vector(unlist(model_deaths_age %>%
                                       group_by(year, fat_age) %>%
                                       summarize(total_value = sum(value)) %>% filter(fat_age == '60+') %>% select(total_value)))
  }
 

  
  # Calculate the SSE
  objective=sum((model_output - target_output)^2)
  
  return(objective)
}


objective_function_new=function(param_value, target_output,age_young) {
  param_Baseline=getModelInputs(scenario="Rubella")
  if(age_young == TRUE){
    # Adjust parameters for young ageing rate
    death_2 = param_Baseline$s[2,]
    param_Baseline$s[2,]=death_2 * param_value 
  } else {
    # Adjust parameters for old ageing rate
    param_Baseline$u[c(62,63)]=rep(param_value, 2)  
  }
  run_out=run_model.D(param_Baseline, initialConditions, timesteps)
  out = run_out$moPostprocessing[[1]] |>
    mutate(age_group=as_factor(age_group))
  
  
  Deaths=out %>% 
    filter(variable == 'deaths_D') %>%
    mutate(source = "mop Deaths") %>%
    select(year, value, source, age_group)
  
  result=data.frame(
    Original_Age_Groups = age_groups,
    Mapped_Age_Groups = mapped_age_groups
  )
  model_deaths_age = Deaths %>% filter(age_group !="All")
  model_deaths_age$fat_age = rep(mapped_age_groups,31)
  if(age_young == TRUE){
    model_output=as.vector(unlist(model_deaths_age %>%
                                    group_by(year, fat_age) %>%
                                    summarize(total_value = sum(value)) %>% filter(fat_age == '0-14') %>% select(total_value)))
  } else {
    model_output=as.vector(unlist(model_deaths_age %>%
                                    group_by(year, fat_age) %>%
                                    summarize(total_value = sum(value)) %>% filter(fat_age == '60+') %>% select(total_value)))
  }
  
  
  
  # Calculate the SSE
  objective=sum((model_output - target_output)^2)
  
  return(objective)
}


################### PARAMETER SPACE ######################################################################################################################################################################################################################## 
param_values=seq(from = 0, to = 1, by = 0.1)

################### RUN OPTIMISATION ################################################################################################################################################################################################################################################
results=sapply(param_values, function(p) objective_function_new(p, target_output_young, age_young = TRUE))

################## INSPECT OPTIMAL PARAMETER ################################################################################################################################################

optimal_param=param_values[which.min(results)]
cat("Optimal parameter value:", optimal_param, "\n")

  # Plot the objective function vs parameter values
plot(param_values, results, type = "b", main = "Objective Function vs Parameter Value",
     xlab = "Parameter Value", ylab = "Objective Function")
abline(v = optimal_param, col = "red", lwd = 2, lty = 2)


####################### RE-RUN MODEL WITH OPTIMAL PARAMTERS ###################################################################
## Ageing upper bound, eats up higher age rates
  # Young ageing rate optim
param_Baseline$u[c(1,2)]=rep(100,2)
print(param_Baseline$u[c(1,2)])

param_Baseline$s[2,] = param_Baseline$s[2,] * optimal_param
  # Old ageing rate optim
param_Baseline$u[c(62,63)]=rep(0.7,2)
print(param_Baseline$u[c(62,63)])
mo_optimal=run_model.D(param_Baseline, initialConditions, timesteps)
mo_baseline = mo_optimal

####################### CHECK PLOTTING OUTPUT ###################################################################
source("output/Plots.R")

########## NEW OBJECTIVE FN for Scalar for 6-12 months##########################################################################################################################################################

