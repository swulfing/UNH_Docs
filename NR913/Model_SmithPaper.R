# JAGS model
sink("smithPaper.txt")
cat(" 
  model {

# PRIORS

for (j in 1:nsite){
  alpha[j] ~ dnorm(mu, tau)
}

#These are our hyperparameters- aka a parameter of a prior distribution as opposed to paramter of a model
  #means and st. devs across all sites
  
mu ~ dnorm(0, 0.001) #ahat from slide
sigma ~ dunif(0,10)
tau <- 1/sigma ^ 2

b_rkm ~ dnorm(0, 1000)
b_netDepth ~ dnorm(0, 1000)
b_distDown ~ dnorm(0, 1000)
b_distUp ~ dnorm(0, 1000)
b_distDetect ~ dnorm(0, 1000)


# LIKELIHOOD

  for (i in 1:ncatch){
    eta[i] <- log(x$deployment_time[i]) #accounts for the unbalanced effort (h) among net series and is equal to the loge duration of time for which net series i was deployed
    
    x$c[i] ~ dpois(lambda[i])
    
    log(lambda[i]) <- eta[i] + alpha[x$site[i]] + b_rkm*x$rkm[i] + b_netDepth*x$netDepth[i] + b_distDown*x$distDown[i] + b_distUp*x$distUp[i] + b_distDetect*x$distDetect[i] #Log Link Function
    
} #end of model
  
 
    ",fill = TRUE)
sink()

# data for the model
win.data <- list(x = data,
                 ncatch = nrow(data)) #Site

# Initial values
inits <- function() list(alpha_hat = rnorm(1),
                         b_rkm = rnorm(1),
                         b_netDepth = rnorm(1),
                         b_distDown = rnorm(1),
                         b_distUp = rnorm(1),
                         b_distDetect = rnorm(1),
                         sigma = runif(1, 0, 10)
)


# Parameters monitored
params <- c("alpha_hat",
            "b_rkm",
            "b_netDepth",
            "b_distDown",
            "b_distUp",
            "b_distDetect",
            "sigma")



# MCMC settings
ni <- 20000
nt <- 1
nb <- 5000
nc <- 3


out <- jags(win.data,
            inits,
            params,
            "smithPaper.txt",
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb,
            working.directory = getwd())

gelman.diag(out, confidence = 0.95, transform=FALSE, autoburnin=TRUE,
            multivariate=TRUE) #Rhat val #This is called a Brooks-Gelman Rubin Stat


