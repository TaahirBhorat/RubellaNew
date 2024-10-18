#Estimating ptrans (probablity of transmission) from age/population group



R0Ptransestimator <- function(R0, pars) {
  lapply(seq(1,N), function(i) {
    with(as.list(pars), {
      npop <- unlist(popage)
      beta <- as.matrix(con_all_prov[[i]]) 
      #    M <- beta0 * psus * (zeta1*pa/r1 + (1-pa)/gamma2 + (1-pa)*pm/r2 + (1-pa)*(1-pm)/taus)
      #  M <- outer(npop, 1/npop) * beta0 * psus * outer(rep(1, G),(zeta1*pa/r1 + (1-pa)/gamma2 + (1-pa)*pm/r2 + (1-pa)*(1-pm)/taus))
      M <- matrix(NA, G, G)
      for(ii in 1:G){
        for(jj in 1:G){
          M[ii,jj] <- (npop[ii]/npop[jj]) * beta[ii,jj] * psus[ii] * (zeta1 * pa / r1 + (1 - pa) / gamma2 + (1 - pa) *pm[jj]/r2 + (1 - pa) * (1 - pm[jj]) / taus)
        }
      }
      rho <- eigen(M, only.values = TRUE)$values
      return(R0/max(abs(rho)))
    })
  }) -> res
  unlist(res)
}

# #Ptrans simple estimator (non population/age class specific)
# R0estimator<-function(R0, pars){
#   with(as.list( pars), {
#     ptrans<-R0/(beta0*(zeta1*pa/(r1*(1-pa)) +mean(pm)/r2+ 1/gamma2 + (1-mean(pm))/taus)*(1-pa))
#   
#     })
# }
