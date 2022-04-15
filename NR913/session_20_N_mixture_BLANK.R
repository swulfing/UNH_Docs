# load library
library(R2jags)

#CH 12 in Kery

# load data ---------------------------------------------------------------

revi <- read.csv("revi_data.csv")


# isolate count data matrix
revi_count <- as.matrix(revi[,2:4], nrow = nrow(revi), ncol = 3) #may have to convert ovserver to factor

# isolate site-level covariate vector
revi_pfor <- as.matrix(revi[,8], nrow = nrow(revi), ncol = 1)
  
# isolate observation covariate matrix
revi_obs <- revi[,5:7]
revi_obs$surv_1_obs <- as.integer(as.factor(substr(revi_obs$surv_1_obs, 4, 4)))
revi_obs$surv_2_obs <- as.integer(as.factor(substr(revi_obs$surv_2_obs, 4, 4)))
revi_obs$surv_3_obs <- as.integer(as.factor(substr(revi_obs$surv_3_obs, 4, 4)))



# null N-mixture model ----------------------------------------------------

# JAGS model 
sink("nmix.txt") #N NEEDS INITS FOR EACH N. 
cat(" 
    model {

    ##############
    ### PRIORS ###
    ##############
    
    beta0 ~ dnorm(0,0.001)  # intercept of forest
    beta1 ~ dnorm(0,0.001)  # effect of forest
    beta2 ~ dnorm(0,0.001)  # intercept of observor
    beta3 ~ dnorm(0,0.001)  # effect of observer
    


    ##################
    ### LIKELIHOOD ###
    ##################
    
    # Abundance Model
    for(i in 1:nsite){
      n[i] ~ dpois(lambda[i]) #n is latent/non obs. therefore needs init val but not a prior?
      log(lambda[i]) <- beta0 + beta1 * forest[i] #log of lambda will be all covariates
      
    # Detection Model
    for(j in 1:nsurvey){
      y[i,j] ~ dbin(p[i,j], n[i]) #y is data, p is detection prob. p needs a prior? p i,j
      logit(p[i,j]) <-  beta2 + beta3 * obs[i,j] #p needs a logit link
    } #j
    } #i

    ##########################
    ### DERIVED PARAMETERS ###
    ##########################
    
  abun <- sum(n[])
    
    } # end of model  
    ",fill = TRUE)
sink()

# data
win.data <- list(y = revi_count,
                   forest = as.numeric(revi_pfor),
                   obs = revi_obs,
                   nsurvey = ncol(revi_count),
                   nsite = nrow(revi_count)
                   )

# Initial values
inits <- function()list(n = rep(5, nrow(revi)))#(Nst = rep(rep(revi_count, nrow(revi))))

# Parameters monitored
params <- c("beta0", "beta1", "beta2", "beta3", "p", "abun") 

# MCMC settings
nc <- 3; nt <- 1; ni <- 3000; nb <- 500

# run the model
out_null <- jags(win.data, inits, params, "nmix.txt", 
                 n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                 working.directory = getwd())
print(out_null, dig = 2)


# N-mixture model with abundance covariate ----------------------------------------------------



# run the model
out_abun <- jags(win.data, inits, params, "nmix.txt", 
                 n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                 working.directory = getwd())
print(out_abun, dig = 2)



# N-mixture model with detection and abundance covariates ----------------------------------------------------



# run the model
out_abun_det <- jags(win.data, inits, params, "nmix.txt", 
                 n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                 working.directory = getwd())
print(out_abun_det, dig = 2)



