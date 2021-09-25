library(deSolve)
library(tidyverse)
library(ggplot2)

#ode(y, times, func, parms, method = "something")

closed.coral.model <- function (t, y, params) {
  ## first extract the state variables
  M <- y[1]
  C <- y[2]
  TT <- y[3]
  P <- y[4]
  X <- y[5]
  ## now extract the parameters
  alpha <- params["alpha"]
  gamma <- params["gamma"]
  r <- params["r"]
  d <- params["d"]
  kappa <- params["kappa"]
  k <- params["k"]
  sigma <- params["sigma"]
  s <- params["s"]
  j <- params["j"]
  psi <- params["psi"]
  phi <- params["phi"]
  ## now code the model equations
  dMdt <- alpha*M*C - (P*M)/(M+TT) + gamma*M*TT
  dCdt <- r*TT*C - d*C - a*M*C
  dTTdt <- (P*M)/(M+TT) - gamma*M*TT - r*TT*C + d*C
  dPdt <- s*P*(1-(P/(k*C))) - sigma*P*(1-X)
  dXdt <- kappa*X*(1-X)*(-1+j*(1-C)-sigma*P*(1-X)+phi *(2*X-1))
  ## combine results into a single vector
  dydt <- c(dMdt,dCdt,dTTdt, dPdt, dXdt)
  ## return result as a list!
  list(dydt)
}


#https://kinglab.eeb.lsa.umich.edu/480/nls/de.html
