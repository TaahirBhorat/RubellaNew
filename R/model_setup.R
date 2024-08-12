# MASHA code structure - co-developed by Lisa White and Sheetal Silal
# Name: Basic SEIR model
# Disease: Malaria
# Time Frame (Data): 2022-01-01 - 2024-12-31

# Notes: 
#---The model structure is written in R.
#---Simulations are run in R with a C compiler.

#### Install and load packages ####
library(tidyverse)
library(deSolve)
library(ggplot2)
library(glue)
library(readxl)
library(data.table)
library(lubridate)
library(Rcpp)
library(here)
library(crayon)

options(tidyverse.quiet = TRUE)
source(here::here('R/getModelInputs.R'))
source(here::here("R/utils.R"))
source(here::here('R/mt_utils.R'))
# LOG <- makeLogger('trace')


#### Rcpp transitions to equations -------------------------------------------
LOG('Compiling CPP')
cppFunction('
  void EQ(NumericVector eq, NumericVector transit, 
          IntegerVector transitionsiu1, IntegerVector transitionsiu2, 
          IntegerVector transitionsiv1, IntegerVector transitionsiv2) {
    int i, iu1, iu2, iv1, iv2;
    // Zero out eq
    eq.fill(0.0);
    // Fill equations with new deltas
    //Rcpp::Rcout << transit.length() << std::endl;
    for(i=0; i<transit.length(); i++) {
      iu1 = transitionsiu1[i]-1;
      iv1 = transitionsiv1[i];
      iu2 = transitionsiu2[i]-1;
      iv2 = transitionsiv2[i];
      
      // Include transition differential in the relevant eq components
      
      eq[iu1] += transit[i]*iv1;
      eq[iu2] += transit[i]*iv2;
      
      //Rcpp::Rcout << "eq["<<iu1<<"] += "<<transit[i]<<"*"<<iv1<<";" << std::endl;
      //Rcpp::Rcout << "eq["<<iu2<<"] += "<<transit[i]<<"*"<<iv2<<";" << std::endl;
    }
  }
')
#### Model start-up definitions ####
N <- 63   # number of patches (age bands)
#### Import Data ####

startyear <- 2000 # starting year of simulation 2015-01-01
endyear <- 2030
tyears <- endyear - startyear # total years of simulation
dtout <- 1/365 # output timestep
timesteps <- startyear+seq(0, tyears, dtout) # time vector

LOG('Prepping data')


