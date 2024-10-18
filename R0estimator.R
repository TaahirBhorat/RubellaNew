### PARAMTERS ###############################################################################################################################################################################################################################################################
library(readxl)
test <- read_excel('parameters/DataWorkbookRubella.xlsx', sheet = 'Initial_conditions_small')
popage <- test$Pop  

con_national <- param_Baseline$contact  

R0Ptransestimator <- function(R0, pars) {
  
  # Unpack parameters for easier access
  with(as.list(pars), {
    npop <- unlist(popage)  # Population size for each age group
    G <- length(npop)       # Number of age groups
    
    # Convert the contact matrix to a matrix format
    beta <- as.matrix(con_national) 
    
    # Initialize a GxG matrix to store the transmission rates
    M <- matrix(NA, G, G)
    
    # Loop over all pairs of age groups to populate the transmission matrix M
    for (ii in 1:G) {
      for (jj in 1:G) {
        M[ii, jj] <- (npop[ii] / npop[jj]) * beta[ii, jj]
      }
    }
    
    # Compute the eigenvalues of the matrix M
    rho <- eigen(M, only.values = TRUE)$values
    
    # Calculate ptrans by dividing R0 by the largest absolute eigenvalue
    ptrans <- R0 / max(abs(rho))
    
    # Return the estimated probability of transmission (ptrans)
    return(ptrans)
  })
}

### RUN THIS ######################################################################################################################################################################################################## 
parameters <- list(
  popage = popage,         # Population sizes
  con_national = con_national  # National-level contact matrix
)

# Run the estimator with R0 = 3
ptrans_result <- R0Ptransestimator(R0 = 5.2, pars = parameters)

# Print the result to see the estimated probability of transmission
print(ptrans_result)
