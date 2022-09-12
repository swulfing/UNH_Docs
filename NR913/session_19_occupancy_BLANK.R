# load library
library(R2jags)

purps <- "#440154FF"
midnight <- "#414487FF"
dkteal <- "#2A788EFF"
limabean <- "#7AD151FF"
bananas <- "#FDE725FF"

ccolors <- c(purps, midnight, dkteal,limabean, bananas)

# load & prep data  ---------------------------------------------------------------

# GOAL: model pine marten distribution and evaluate habitat relationships
# SUBGOAL: how much does imperfect detection affect inference?

# load data
mart <- read.csv("marten_data_xy_det.csv")
mart <- mart[!(is.na(mart[,5])),] # remove some rows to simplify the model

# naive occupancy NEED REPREATED SAMPLING FOR OCCUPANCY MODELS
names(mart)
dets <- mart[,5:10] # isolate detection/non-detection data
naive_occ <- apply(dets, 1, max, na.rm = TRUE)#not accounting for imperfect detection
           

# create scaled versions of covariates
mart$ARABLE  <- scale(mart$arable)[,1]
mart$BWOOD <- scale(mart$bwoodland)[,1]
mart$CWOOD <- scale(mart$cwoodland)[,1]
mart$DBLG <- scale(mart$urban)[,1]


# "null" occupancy model  -------------------------------------------------

# JAGS model 
sink("occ.txt")
cat(" 
    model {
    ##############
    ### PRIORS ###
    ##############
    
    psi ~ dunif(0,1) #not normal. psi is a prob
    p ~ dunif(0,1)

    ##################
    ### LIKELIHOOD ###
    ##################
    
    # OCCUPANCY SUBMODEL - ECOLOGICAL PROCESS - no direct data being applied to this one
    
    for(i in 1:nsite){
        z[i] ~ dbern(psi) #z is going to be occupancy. z is true latent (i.e. not observed) state of occupancy
        #z needs inital val but now a prior
        
    # DETECTION SUBMODEL - OBSERVATION PROCESS
    
        for(j in 1:nsurvey){
            y[i,j]~dbern(p * z[i]) #y is the detection prob
    
        } # j
    } # i
    
    # DERIVED PARAMS 
    sites.occ <- sum(z[])
    

#psi-prob of occupancy
#p-prob of detection GIVEN occupancy (z). therefore need to multiply it by z




    } # end of model  
    ",fill = TRUE)
sink()

# data
win.data <- list(nsite = nrow(mart),
                 nsurvey = ncol(dets),
                 y = dets)

# Initial values
inits <- function()list(z = rep(1, nrow(mart))) #Don't want it to say unoccupied when it is

# Parameters monitored
params <- c("psi","p", "sites.occ", "z") 

# MCMC settings
nc <- 3; nt <- 1; ni <- 3000; nb <- 500

# run the model
out_null <- jags(win.data, inits, params, "occ.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
            working.directory = getwd())
print(out_null, dig = 2)
#rhat is 1, p is det probability (.34)
#psi 51%
#sites.occ no of occupied sites (51% of sites)
#if there is a detection, z will be 1. If not, each z assigns a prob that the model thinks that it's occupied


#  occupancy model with occ covariates (out_cov) -------------------------------------------------

sink("occ_cov.txt")
cat(" 
    model {
    ##############
    ### PRIORS ###
    ##############
    
    beta.int ~ dlogis(0,1)
    beta.arable ~ dlogis(0,1)
    beta.bwood ~ dlogis(0,1)
    beta.cwood ~ dlogis(0,1)
    beta.dblg ~ dlogis(0,1)
    
    p ~ dunif(0,1)

    ##################
    ### LIKELIHOOD ###
    ##################
    
    # OCCUPANCY SUBMODEL - ECOLOGICAL PROCESS - no direct data being applied to this one
    
    for(i in 1:nsite){
        z[i] ~ dbern(psi[i]) #z is going to be occupancy. z is true latent (i.e. not observed) state of occupancy
        #z needs inital val but not a prior
        logit(psi[i]) <- beta.int
        + beta.arable * arable[i]
        + beta.bwood * bwood[i]
        + beta.cwood * cwood[i]
        + beta.dblg * dblg[i]
        
    # DETECTION SUBMODEL - OBSERVATION PROCESS
    
        for(j in 1:nsurvey){
            y[i,j]~dbern(p * z[i]) #y is the detection prob
    
        } # j
    } # i
    
    # DERIVED PARAMS 
    sites.occ <- sum(z[])
    

#psi-prob of occupancy
#p-prob of detection GIVEN occupancy (z). therefore need to multiply it by z




    } # end of model  
    ",fill = TRUE)
sink()

# data
win.data <- list(nsite = nrow(mart),
                 nsurvey = ncol(dets),
                 arable = mart$ARABLE,
                 bwood = mart$BWOOD,
                 cwood = mart$cwood,
                 dblg = mart$DBLG,
                 y = dets)

# Initial values
inits <- function()list(z = rep(1, nrow(mart)),
                        beta.int = rlogis(1,0,1)) #Don't want it to say unoccupied when it is

# Parameters monitored
params <- c("psi","p", "sites.occ",
            "beta.int", "beta.arable", "beta.bwood", "beta.cwood", "beta.dblg") 

# MCMC settings
nc <- 3; nt <- 1; ni <- 3000; nb <- 500

# run the model
out_cov <- jags(win.data, inits, params, "occ_cov.txt", 
                 n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                 working.directory = getwd())
print(out_cov, dig = 2)
#rhat is 1, p is det probability (.34)
#psi 51%
#sites.occ no of occupied sites (51% of sites)
#if there is a detection, z will be 1. If not, each z assigns a prob that the model thinks that it's occupied




#  occupancy model with occ covariates + RI (out_ri) -------------------------------------------------
#random intercept


#  occupancy model with occ covariates + RI + trap happy (out_ri_th) -------------------------------------------------

 -------------------------------------------------
# p = detection prob when not detected in previous week
# q = detection prob when detected in previous week
#     if marten is trap happy, q>p

    
sink("occ_cov_th.txt")

cat(" 
    model {
    ##############
    ### PRIORS ###
    ##############
    
    beta.int ~ dlogis(0,1)
    beta.arable ~ dlogis(0,1)
    beta.bwood ~ dlogis(0,1)
    beta.cwood ~ dlogis(0,1)
    beta.dblg ~ dlogis(0,1)
    
    p ~ dunif(0,1)
    q ~ dunif(0,1)

    ##################
    ### LIKELIHOOD ###
    ##################
    
    # OCCUPANCY SUBMODEL - ECOLOGICAL PROCESS - no direct data being applied to this one
    
    for(i in 1:nsite){
        z[i] ~ dbern(psi[i]) #z is going to be occupancy. z is true latent (i.e. not observed) state of occupancy
        #z needs inital val but not a prior
        
        logit(psi[i]) <- beta.int
        + beta.arable * arable[i]
        + beta.bwood * bwood[i]
        + beta.cwood * cwood[i]
        + beta.dblg * dblg[i]
        
    # DETECTION SUBMODEL - OBSERVATION PROCESS
    
    y[i,1] ~ dbern(z[i] * p)
    
        for(j in 2:nsurvey){
            y[i,j]~dbern(p.eff[i,j] * z[i]) #y is the detection prob
            p.eff[i,j] <- p * (1 - y[i,j-1]) + q * y[i,j-1]
    
        } # j
    } # i
    
    # DERIVED PARAMS 
    sites.occ <- sum(z[])
    

#psi-prob of occupancy
#p-prob of detection GIVEN occupancy (z). therefore need to multiply it by z




    } # end of model  
    ",fill = TRUE)
sink()

# data
win.data <- list(nsite = nrow(mart),
                 nsurvey = ncol(dets),
                 arable = mart$ARABLE,
                 bwood = mart$BWOOD,
                 cwood = mart$cwood,
                 dblg = mart$DBLG,
                 y = dets)

# Initial values
inits <- function()list(z = rep(1, nrow(mart)),
                        beta.int = rlogis(1,0,1)) #Don't want it to say unoccupied when it is

# Parameters monitored
params <- c("psi","p", "sites.occ", "q",
            "beta.int", "beta.arable", "beta.bwood", "beta.cwood", "beta.dblg") 

# MCMC settings
nc <- 3; nt <- 1; ni <- 3000; nb <- 500

# run the model
out_cov_th <- jags(win.data, inits, params, "occ_cov_th.txt", 
                n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                working.directory = getwd())
print(out_cov_th, dig = 2)
#rhat is 1, p is det probability (.34)
#psi 51%
#sites.occ no of occupied sites (51% of sites)
#if there is a detection, z will be 1. If not, each z assigns a prob that the model thinks that it's occupied





#  occupancy model with occ covariates + RI + quad conifer (out_ri_2 -------------------------------------------------


