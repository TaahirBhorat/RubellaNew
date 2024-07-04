# MASHA code structure - stupid rabbit
#install.packages("remotes")
#remotes::install_github("nick-moffitt/orderlabel")
library(readxl)
source("R/mt_utils.R")
source("R/utils.R")
LOG = makeLogger()
source("R/model_setup.R")
library(tidyverse)
modelFilename <- 'RubellaModel.xlsx'
#### Ensure bigX exists #### 
# mt_updateBigX(modelLetter=modelLetter,
#               overwrite = T)
# To update shinyInputs: mt_updateShinyInputs
# To update parameter sheet: mt_updateParameters(overwrite=T)

#### Read mtMod_D in ####
mtMod_D <- mt_getTibblesInList(modelFilename=modelFilename)

tbCompartments_D <- mtMod_D$tbCompartments
tbTransitions_D <- mtMod_D$tbTransitions
alivepop_D <- mtMod_D$alivepop

varind_D <- mtMod_D$varind
traind_D <- mtMod_D$traind
#Define state names for solver 
symbols_D <- mtMod_D$tbCompartments %>% pull(id, name=State)
getStates.D <- function(x) {
  popc <- .colSums(x[varind_D[alivepop_D,]], m=length(alivepop_D), n=N)
  states <- lapply(symbols_D, function(id){x[varind_D[id,]]})
  states$D.pop <- c(popc)
  return(states)
}

#### Define transitions_D #### 
# ************************************************************************************* #

# transitions_D will be a matrix with 4 columns:
# columns 1,2 describe the source of transitions
# columns 3,4 describe the destination
# columns 1,3 describe the compartment
# columns 2,4 describe if this transition is coming, leaving or both
transitionCreationExpressions_D <- mt_makeTransitionCreationCode(mtMod_D, 
                                                               modelLetter='D') %>% 
  parse(text=.)
# TO PRINT THE CODE USED TO CONSTRUCT TRANSITIONS `transitions_D`:
# as.character(transitionCreationExpressions_D)%>%paste0(collapse='\n')%>%cat
transitions_D <- matrix(0, nrow=N*nrow(tbTransitions_D), ncol=4)
for (n in 1:N) {
  nxt <- min(n+1, N)  # nxt used in aging, for the last age group nxt should just be n
  # Evaluate each of the transition creation expressions in the current environment
  curEnv <- environment()
  transitionCreationExpressions_D %>% 
    eval(envir = curEnv)
}



# ************************************************************************************* # #YES
#### Base functions ####

# ************************************************************************************* #
# Function to calculate transition rates, given variables and parameters
# ************************************************************************************* #
# transitions function
disrates.D <- function(x, parameters, t) {
  with(as.list(c(parameters, getStates.D(x), c(mtMod_D$tbAges[,'agerate']))), {
    
    #time filters 
    t_internal<-t#+startyear
    tic<-as.character(floor(t_internal) )
    #if(!(tic %in% colnames(covm))) browser()
    #  if(sum(D.V_1<0)>0) browser()
    # if(t_internal>2025.87) browser()
    
    #print(paste("Time:", t_internal, "D.pop:", D.pop, "Infectious:", I))
    
    #parameters

    infectious_D <- (I)/D.pop
    lambda = 1*ptrans*as.vector(contact%*%infectious_D)
    
    
    
    
    
    #TO DO: Remove when model is fully verified
    #Debugging
    #Vaccine start conditions set to 0
    #cov[,tic]<-rep(0, N)
    # mov_D[,tic]<-1
    #mrate=0
    #totbirths <- deathprop <- rep(0,N)
    
    #lambda_D<-rep(0, N)
    #tau_vmm_D[tic]<-0
    #tau_mi_D[tic]<-tau_1_D[tic]<-tau_2_D[tic]<-tau_3_D[tic]<-tau_b_D[tic]<-tau_cb_D[tic]<-tau_ab_D[tic]<-tau_vmm_D[tic]<-0
    #agerate<-rep(0, N)
    
    
    #### TO MAKE RATES CODE: #
    #mt_makeTransitionRatesCode(mtMod_D) %>% cat
    #### Check symbols defined
    
    # view(mt_findSymbols(mtMod_D, ls()))
    # T1 <- Sys.time()
    # RATES (PASTED) ####
    
    #TO DO:  Jared to edit output to shinyapps to add cov1yr, cov2yr etc as a matrix  with colnames of year. 
    # then replace code below with 
    #cov1<-cov1yr[,tic]
    #cov2<-cov2yr[,tic]
    #cov3<-cov3yr[,tic]

    
    

    
    #delta_D<-365.25/3.5 #NICD https://www.nicd.ac.za/wp-content/uploads/2017/03/NICD-guidelines_diphtheria_v3_28-May-2018.pdf
    #omega_D<-365.25/3 # upper bound of parameter
    #phi_D <- 365.25/5 #range is 2 (0-33) for Rohingya response. should be higher in non-emergency setting

# Vaccination from M, R as well
    bi <- rep_along(S, 0)
    bi[1] <- births[[tic]]
  # if (tic =='2023'){
  #   browser()
  # } 
    tranrate <- array(c(
      (1-mprop)*bi,  # Births no-maternal-immunity
      mprop*bi,  # Births Maternal Immunity
      d*M,  # Loss of maternal immunity
      (1-s[,tic])*v[,tic]*u*S,  # ageing, vaccination from succeptible
      lambda*S,  # infection
      gamm*I,  # natural recovery
      (1-v[,tic])*(1-s[,tic])*u*M,  # ageing, no vaccination from maternal immune
      (1-s[,tic])*(1-v[,tic])*u*S,  # ageing, no vaccination from succeptible
      (1-s[,tic])*u*V,  # ageing in vaccination compartment
      (1-s[,tic])*u*(1-v[,tic])*M,  # ageing maternal immunity loss
      (1-s[,tic])*v[,tic]*u*M,  # ageing, vaccination from maternal immune
      (1-s[,tic])*v[,tic]*u*R,  # Recovered Vaccination
      s[,tic]*M,  # natural death
      s[,tic]*S,  # natural death
      s[,tic]*I,  # natural death
      s[,tic]*R,  # natural death
      s[,tic]*V,  # natural death
      (1-v[,tic])*(1-s[,tic])*u*R  # Recovered ageing
    ), dim=c(N, 18))
    tranrate <- c(t(tranrate))
    #browser()
  if(any(is.na(tranrate)) == TRUE){
    browser()
  }
    # T2 <- Sys.time()
    return(tranrate)
  })
}

# POST PROCESSING function
# Assumes dfAge/N/traind_D/varind_D exists
postproc.D  <- function(parameters, out, tran) {
  with(as.list(c(parameters)), {
    # ************************************************************************************* #
    # for outputting the  time series for each patch
    # ************************************************************************************* #
    ### Case outputs ####
    # REMEMBER to modify BOTH commented sections 1 and 2 below:
    # 1 - Add your postproc variable names
    
    # Add what you want to count here
    postprocVars <- vars(popa,
                         imune_D,
                         all_Inc_D,
                         doses_1_D)
    
    postprocVarNames <- postprocVars %>% sapply(rlang::as_name)
    postprocVarList <- lapply(postprocVarNames, function(varName){
      matrix(0, nrow = length(out[, 1]), ncol = N)
    })
    names(postprocVarList) <- postprocVarNames

  
    #Count the things
    #Grouping Transitions for use for counting 
    allincTransitions <- tbTransitions_D %>% filter(To%in%c('I[n]')) %>% pull(id)
    imuneCompartments <- c('M',"V",'R')

    doses_1Transitions <- tbTransitions_D %>% filter(To=='V[nxt]') %>% pull(id)



    
    # Here do the actual age stratified counting 
    # Model timestep in years, get it per day(/365)
    # try 365.25
    for (n in 1:N) {
      # 2 - Fill variables with values for each N
      popa <- rowSums(out[,c(varind_D[c(alivepop_D),n])+1])  # Alive population
      postprocVarList$popa[, n]  <- popa
      postprocVarList$imune_D[, n] <- rowSums(out[, c(varind_D[imuneCompartments, n])+1])
      postprocVarList$all_Inc_D[, n]  <- (tran[, unname(traind_D[allincTransitions, n])] / 365)     # All Incidence
      postprocVarList$doses_1_D[, n]  <- rowSums(tran[, traind_D[doses_1Transitions, n]] / 365)     # Doses
       # Prevalence
    }
    # Ignore from here down
    timesteps <- out[, 1, drop = T]
    formatAsTibble <- function(varName) {
      df <- postprocVarList[[varName]]
      colnames(df) <- 1:N
      as_tibble(df) %>%
        mutate(time = timesteps) %>%
        pivot_longer(!time, names_to = 'age', values_to = varName) %>%
        mutate(age=as.integer(age), age_group = as_factor(mtMod_D$tbAges$age_group[age])) %>% 
        select(!age)
    }
    df <- as_tibble(expand.grid(age_group = as_factor(mtMod_D$tbAges$age_group[1:N]),
                                time = timesteps)) %>%
      select(time, age_group)
    
    for (i in seq_along(postprocVarList)) {
      varName <- postprocVarNames[[i]]
      df <- df %>%
        left_join(formatAsTibble(varName), by=c('time','age_group'))
    }
    # Return nicely formatted tibble
    df %>%
      pivot_longer(!c(time,age_group), names_to='variable')
  })
}


# EQUATION COMPILER
epiModel.D <- function(t, state, parameters) {
    # rates of change
  transit <- disrates.D(state, parameters, t)
  EQ(dZ, transit, transitions_D[,1], transitions_D[,3], transitions_D[,2], transitions_D[,4])

  list(c(dZ))
}

#Seroprevalence: Use data for warm up period
#Make calibration data for seroprevalence.
# M starts 0
# V at 0
# Seroprev for each month 
###Only really need to set initial age conditions for S,I and R
### Take 2018 positives as my I divided by age groups
#### Look at number of cases by age group take the positives times by populations and put in R and S should the opposite(the negatives)
#### Check no vac, no disease do they age correctly!  



# RUN FUNCTION  ####
run_model.D <- function(parameters, initialConditions, timesteps,
                        returnRawModel = T, returnPostprocessedModel = T) {
  # ************************************************************************************* 
  if (!returnRawModel && !returnPostprocessedModel) {
    LOG("Not sure why you're running a model without asking for anything back...")
    return(NULL)
  }
  # If returnRawModel and returnPostprocessedModel are both true then return a
  # list with names moRaw and moPostprocessing:
  #   `moRaw` is the raw output
  #   `moPostprocessing` is a list with the tibble containing postprocessed outputs as the only element
  # The reason it's a list is because initially we were gonna have many things in there but
  # We later decided to just have one thing. The bulk of the code assumes this structure now.
  #Solve ODE
  dZ <<- as.numeric(rep(0.0, N*nrow(tbCompartments_D)))
  LOG("Solving the ODE for Diphtheria with ptrans={parameters$ptrans_D}", LEVEL$TRACE)
  outoderun <- ode(y=initialConditions, times=timesteps, func=epiModel.D, method = "euler", 
                   parms=parameters)
  LOG("Diphtheria solver diagnostics: {diagnostics(outoderun)}", LEVEL$TRACE)
  
  # Return just the raw model output:
  if(returnRawModel && !returnPostprocessedModel) {
    LOG("Returning *just* the raw model output object...")
    return(outoderun)
  }
  
  LOG("Calculating transition values for Diphtheria now", LEVEL$TRACE)
  # Compute transitions at each time step
  tranoderun <- matrix(0,
                       nrow=length(timesteps),
                       ncol=N*nrow(tbTransitions_D))
  for (ti in seq_along(timesteps)) {
    compartmentValues <- outoderun[ti,2:(N*nrow(tbCompartments_D)+1)]
    tranoderun[ti,]<-t(disrates.D(x=compartmentValues,
                                  parameters=parameters,
                                  t=timesteps[[ti]]))
  }

  LOG("Postprocessing Diphtheria", LEVEL$TRACE)
  ppout <- postproc.D(parameters, outoderun, tranoderun)

  ppout|>
    filter(age_group == first(age_group))|>
    ggplot() +
    aes(
      x = time,
      y = value,
      colour = variable,
      group = variable
    ) +
    geom_point() +
    scale_color_hue(direction = 1) +
    theme_minimal()
  DISoutage <- ppout %>% 
    mutate(disease="Diphtheria",
           unit = "Daily") #Daily outputs by age group


  
  # This object takes a second or two but is useful in saving time in further calculations
  DISoutageVT <- ppout %>%
    filter(str_ends(variable,'100k_D',negate=T)) %>%
    mutate(year=floor(time),
           varType=case_when(
             variable=="prev_D" ~ 'prev',
             variable=="protected_D" ~ 'prev',
             variable=='popa' ~ 'pop',
             TRUE~'general') %>%
             factor(levels=c('general','prev','pop'))) %>%
    arrange(time,age_group,varType)
  
  DISoutallyr <- postProcSummarizeAgesYear.D(DISoutageVT)  # Annual outputs for all ages
  
  DISoutageyr <- postProcSummarizeYear.D(DISoutageVT)  # Annual outputs by age group

  DISoutyr <- bind_rows(DISoutageyr, DISoutallyr) %>% 
    mutate(disease="Diphtheria",
           unit = "Annual") %>% 
    select(year, age_group, variable, disease, value, unit)
  
  LOG("Completed Diphtheria, returning tibble with unit=Annual", LEVEL$TRACE)
  
  moPostprocessing = list(DISoutyr)
  if (returnRawModel && returnPostprocessedModel) {
    LOG("Returning moRaw and moPostprocessing for Diphtheria")
    return(list(moRaw=outoderun,
                moPostprocessing=moPostprocessing))
  } else {
    LOG("Returning *just* moPostprocessing for Diphtheria")
    return(moPostprocessing)
  }
}

postProcSummarize.D <- function(DISoutage) {
  
  popa<- DISoutage %>% 
    filter(variable =="popa") %>% 
    rename(popa = value)
  
  DISoutage %>%
    filter(variable%in%c('prev_D', 'protected_D')) %>% 
    ungroup() %>% 
    left_join((popa %>% select(time,age_group, popa)), by=c("time", "age_group")) %>% 
    mutate(value = value/popa) %>% 
    select(time, variable,age_group, value, disease, unit) %>%
    transmute(time=time,
              age_group = age_group,
              variable=str_replace(variable,'_D','_prop_D'),
              value = value,
              disease=disease, 
              unit=unit) 
}

postProcSummarizeAges.D <- function(DISoutage) {
  
  pop <- DISoutage %>% 
    filter(variable =="popa") %>% 
    group_by(time, variable, disease) %>% 
    summarise(pop = sum(value)) %>% 
    mutate(variable = "pop")
  
  sum1 <- DISoutage %>%
    filter(str_ends(variable, '100k_D', negate = T) ) %>% 
    group_by(time, variable, disease) %>%
    summarise(value = sum(value)) 
  
  
  sum2 <- sum1 %>% 
    left_join((pop %>% select(time,pop)), by=c("time")) %>% 
    mutate(per100k = value/pop*100000) %>% 
    transmute(time=time,
              variable=str_replace(variable.x,'_D','100k_D'),
              disease=disease,
              value=per100k) %>% 
    filter(!variable%in%c('popa'))
  
  
  sum3<- sum1 %>% 
    filter(variable%in%c('prev_D', 'protected_D')) %>%
    left_join((pop %>% select(time,pop)), by=c("time")) %>% 
    mutate(value = value/pop) %>% 
    transmute(time=time,
              variable=str_replace(variable.x,'_D','_prop_D'),
              value = value,disease=disease) %>% 
    filter(!variable%in%c('popa')) %>% 
    select(time, variable, value, disease)
  
  
  bind_rows(sum1, sum2, sum3, (pop %>% rename(value = pop))) %>% mutate(age_group = "All",
                                                                        unit = "Daily")
}

postProcSummarizeAgesYear.D <- function(DISoutageVT) {
  popyr<- DISoutageVT %>% 
    filter(varType == "pop") %>% 
    group_by(time, variable) %>% 
    summarise(pop = sum(value),.groups='keep') %>% 
    group_by(year=floor(time)) %>% 
    summarise(pop = mean(pop), .groups='drop')
  
  # general
  sum1yr<- DISoutageVT %>%
    filter(varType=='general') %>% 
    group_by(year, variable) %>% 
    summarise(value = sum(value), .groups='drop')
  
  # per100k
  sum2yr<- sum1yr %>% 
    left_join(popyr, by="year") %>%
    mutate(per100k = value/pop*100000) %>% 
    transmute(year=year,
              variable=str_replace(variable,'_D$','100k_D'),
              value=per100k)
  
  # prevalence
  sum3yr<- DISoutageVT %>%
    filter(varType=='prev') %>% 
    summarise(value = sum(value),.by=c(year, time, variable)) %>%
    summarise(value = mean(value),.by=c(year, variable)) %>% 
    left_join((popyr %>% select(year, pop)), by=c("year")) %>% 
    mutate(value = value/pop) %>%
    select(year, variable, value) %>%
    transmute(year=year,
              variable=str_replace(variable, '_D$', '_prop_D'),
              value = value)
  
  sum4yr = popyr %>%
    transmute(year, variable='pop_D', value=pop)
  
  bind_rows(sum1yr, sum2yr, sum3yr, sum4yr) %>% mutate(disease="Diphtheria",
                                                       age_group = "All",
                                                       unit = "Annual")
}

postProcSummarizeYear.D <- function(DISoutageVT) {
  popyr <- DISoutageVT %>% 
    filter(variable =="popa") %>% 
    group_by(year=floor(time), age_group, variable) %>% 
    summarise(pop = mean(value), .groups='drop') %>%
    select(year,age_group,pop)
  
  sum1yr <- DISoutageVT %>%
    filter(varType=='general') %>% 
    group_by(year, age_group, variable) %>%
    summarise(value = sum(value), .groups='drop')
  
  sum2yr <- sum1yr %>% 
    left_join(popyr, by=c("year", "age_group")) %>%
    transmute(year=year,
              age_group = age_group,
              variable=str_replace(variable,'_D$','100k_D'),
              value=value*1e5/pop)
  
  sum3yr<- DISoutageVT %>%
    filter(varType=='prev') %>% 
    group_by(year, age_group, variable) %>%
    summarise(value = mean(value), .groups='drop') %>%
    left_join(popyr, by=c("year", 'age_group')) %>% 
    mutate(value = value/pop,
           variable = str_replace(variable,'_D$','_prop_D')) %>% 
    select(year, age_group, variable, value)
  
  sum4yr = popyr %>%
    transmute(year=year,
              age_group=age_group,
              variable='pop_D',
              value=pop,
              disease="Diphtheria")
  
  bind_rows(sum1yr, sum2yr, sum3yr, sum4yr) %>% mutate(unit = "Annual")
}

# Plotting Functions ####
# Postprocessing
pltPostProc.D  <- function(mo, var='inc_pred') {
  mo %>%
    mutate(Date=lubridate::date_decimal(time)) %>% 
    filter(variable==var) %>% 
    ggplot() +
    aes(x = Date, y=value) +
    geom_line(size = 1.5) +
    theme_minimal() +
    facet_wrap(vars(as_factor(age_group)),scales = 'free_y') +
    labs(title=as.character(glue::glue("Diphtheria {var} plot per age group")))
}

pltPostProc_yr.D  <- function(mo, var='inc_pred') {
  mo %>%
    mutate(Date=lubridate::date_decimal(year)) %>% 
    filter(variable==var) %>% 
    ggplot() +
    aes(x = Date, y=value) +
    geom_col(linewidth = 1.5) +
    theme_minimal() +
    facet_wrap(vars(as_factor(age_group)),scales = 'free_y') +
    labs(title=as.character(glue::glue("Diphtheria {var} plot per age group")))
}

