library(deSolve)
library(tidyverse)
library(ggplot2)

#https://kinglab.eeb.lsa.umich.edu/480/nls/de.html
#ode(y, times, func, parms, method = "something")

closed.coral.model1 <- function (t, y, params) {
  ## first extract the state variables
  M <- y[1]
  C <- y[2]
  P <- y[3]
  X <- y[4]
  #TT <- y[5]
  
  ## now extract the parameters
  alpha <- params["alpha"]
  gamma <- params["gamma"]
  r <- params["r"]
  d <- params["d"]
  kappa <- params["kappa"]
  sigma <- params["sigma"]
  s <- params["s"]
  j <- params["j"]
  phi <- params["phi"]
  TT <- 1 - M - C
  
  ## now code the model equations
  dMdt <- alpha*M*C - (P*M)/(M+TT) + gamma*M*TT
  dCdt <- r*TT*C - d*C - alpha*M*C
  #dTTdt <- (P*M)/(M+TT) - gamma*M*TT - r*TT*C + d*C
  dPdt <- s*P*(1-(P/C)) - sigma*P*(1-X)
  dXdt <- kappa*X*(1-X)*(-1+j*(1-C)-sigma*P*(1-X)+phi *(2*X-1))
  
  ## combine results into a single vector
  dydt <- c(dMdt,dCdt, dPdt, dXdt)
  ## return result as a list!
  
  list(dydt)
}

closed.coral.model2 <- function (t, y, params) {
  ## first extract the state variables
  M <- y[1]
  C <- y[2]
  P <- y[3]
  X <- y[4]
  #TT <- y[5]
  
  ## now extract the parameters
  alpha <- params["alpha"]
  gamma <- params["gamma"]
  r <- params["r"]
  d <- params["d"]
  kappa <- params["kappa"]
  sigma <- params["sigma"]
  s <- params["s"]
  j <- params["j"]
  phi <- params["phi"]
  z <- params["z"]
  TT <- 1 - M - C
  
  ## now code the model equations
  dMdt <- alpha*M*C - (P*M)/(M+TT) + gamma*M*TT
  dCdt <- r*TT*C - d*C - alpha*M*C
  #dTTdt <- (P*M)/(M+TT) - gamma*M*TT - r*TT*C + d*C
  dPdt <- s*P*(1-(P/(1-z*C))) - sigma*P*(1-X)
  dXdt <- kappa*X*(1-X)*(-1+j*(1-C)-sigma*P*(1-X)+phi *(2*X-1))
  
  ## combine results into a single vector
  dydt <- c(dMdt,dCdt, dPdt, dXdt)
  
  ## return result as a list!
  list(dydt)
}

#paper changes sigma and s
parms1 <- c(alpha = .1, gamma = .8, r = 1, d = .44, kappa = 1.014, sigma = .5, s = .49, j = 1.68, phi = .2)
parms2 <- c(alpha = .1, gamma = .8, r = 1, d = .44, kappa = 1.014, sigma = .5, s = .49, j = 1.68, phi = .2, z = .5)

times <- seq(from = 0, to = 100, by = 1)
xstart <- c(M = .3, C = .6, P = .2, X = .1) #No initial condition listed for TT

ode(
  func=closed.coral.model1,
  y=xstart,
  times=times,
  parms=parms1
) %>%
  as.data.frame() -> out1

out1 %>%
  gather(variable,value,-time) %>%
  ggplot(aes(x=time,y=value,color=variable))+
  geom_line(size=2)+
  theme_classic()+
  labs(x='time (yr)',y='pop')

ode(
  func=closed.coral.model2,
  y=xstart,
  times=times,
  parms=parms2
) %>%
  as.data.frame() -> out2

out2 %>%
  gather(variable,value,-time) %>%
  ggplot(aes(x=time,y=value,color=variable))+
  geom_line(size=2)+
  theme_classic()+
  labs(x='time (yr)',y='pop')


