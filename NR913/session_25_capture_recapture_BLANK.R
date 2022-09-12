library(R2jags)


# load and inspect data
# for mark/recapture need some kind of replication whether in space or in time
tigers <- read.csv("tigers.csv")

# min number of tigers
min_tig <- nrow(tigers)

# total number of detections
sum(tigers)

# naive detection probability. Tot tigers over tot obs
det_prob <- sum(tigers)/sum(!(is.na(tigers)))

# probability of detecting a tiger at least once over 10 occasions
prob_det_1 <- 1 - ((1 - det_prob)^10)

# simplest estimate of abundance
min_tig/prob_det_1


# lincoln-peterson estimator ----------------------------------------------

# first, get minimum number detected at each 2 occasion pairing
# define a function to do this
naive_est <- function(occ1, occ2){
    estimate <- sum(occ1 + occ2 > 0)
    return(estimate)
    
}
naive_est(tigers$occ01,tigers$occ02)
naive_est(tigers$occ02,tigers$occ03)

# define a function for the simple L-P estimator
lp <- function(occ1, occ2){
  n1 <- sum(occ1)
  n2 <- sum(occ2)
  m2 <- sum(occ1 + occ2 == 2)
  estimate <- (n1 * n2)/m2
  return(estimate)
}

lp(occ1 = tigers$occ01, occ2 = tigers$occ02)

# define a function fof L-P estimator for small sample sizes
lp_small <- function(occ1, occ2){
  n1 <- sum(occ1)
  n2 <- sum(occ2)
  m2 <- sum(occ1 + occ2 == 2)
  estimate <- ((n1+1) * (n2+1))/(m2+1) - 1
  return(estimate)
}

lp_small(occ1 = tigers$occ01, tigers$occ02)

# loop through and get L-P estimates for each pairing of occasions
n_ests <- NA
lp_ests <- NA
lp_small_ests <- NA

# loop
for(i in 1:(ncol(tigers)-1)){
  
 n_ests[i] <- naive_est(tigers[,i], tigers[,i-1])
 lp_ests[i] <-  lp(tigers[,i], tigers[,i-1])
 lp_small_ests[i] <-  lp_small(tigers[,i], tigers[,i-1])
 
}

# look at results
par(mfrow = (c(1,3)))
mean(n_ests)
hist(n_ests)
abline(v = mean(n_ests), lwd = 3, col = "red", lty = 2)

lp_ests <- lp_ests[!(is.infinite(lp_ests))]
mean(lp_ests)
hist(lp_ests)
abline(v = mean(lp_ests), col = "red", lwd = 3, lty = 2)


mean(lp_small_ests)
hist(lp_small_ests)
abline(v = mean(lp_small_ests), col = "red", lwd = 3, lty = 2)

dev.off()

# JAGS simplest closed model ---------------------------------------------------

# Otis 1978

# called "M0"

### ASSUMPTIONS ###

# terminology note: detections = captures

# 1. population is closed
# 2. no animals lose their marks
# 3. no false positives (animals correctly identified)
# 4. each animal has a non-zero probability of being detected
# 5. all animals have equal detection probability  
# 6. detections are independent

# first, augment the data with all-zero detection histories
all_zeros <- matrix(0,nrow = 50,ncol = ncol(tigers))
colnames(all_zeros) <- colnames(tigers)
y_aug <- rbind(tigers,all_zeros)

sink("cap_recap.txt")
cat("
    model {
    
    # PRIORS
      p ~ dunif(0,1)
      omega ~ dunif(0,1)

    # LIKELIHOOD
    for(i in 1:M){
        w[i] ~ dbern(omega)
    
        for(j in 1:nocc){
          y_aug[i,j] ~ dbern(w[i] * p)

        }#j
      }#i


    # DERIVED QUANTITIES
      N <- sum(w[])
    
    }
    ",fill = TRUE)
sink()

win.data <- list(y_aug = y_aug,
                 M = nrow(y_aug),
                 nocc = ncol(y_aug))

# Initial values
wst <- rep(1, nrow(y_aug))  	
inits <- function()list(w = rep(1, nrow(y_aug)) ,
                         p = runif(1,0,1))

# Parameters monitored
params <- c("omega", "p", "N")

# MCMC settings
ni <- 5000; nt <- 1; nb <- 500; nc <- 3

# Call JAGS from R 
out <- jags(win.data,inits, params, "cap_recap.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb,
            DIC = TRUE)
print(out)
#High deviance is bad. DIC uses deviance that lets you compare different models (want lower)
#DIC also penalizes you for model complexity
#DIC is only comparative

# look at diagnostics
library(mcmcOutput)
outB <- mcmcOutput(out, default='N')
diagPlot(outB)

plot(outB)


# JAGS time-varying closed model ---------------------------------------------------

# Otis 1978

# called "Mt"

### ASSUMPTIONS ###

# terminology note: detections = captures

# 1. population is closed
# 2. no animals lose their marks
# 3. no false positives (animals correctly identified)
# 4. each animal has a non-zero probability of being detected
# 5. all animals have equal detection probability  
# 6. detections are independent [now conditional on occasion]. We are now relaxing this

sink("cap_recap.txt")
cat("
    model {
    
    # PRIORS
    for(j in 1:nocc){
      p[j] ~ dunif(0,1)
    }
      omega ~ dunif(0,1)

    # LIKELIHOOD
    for(i in 1:M){
        w[i] ~ dbern(omega)
    
        for(j in 1:nocc){
          y_aug[i,j] ~ dbern(w[i] * p[j])

        }#j
      }#i


    # DERIVED QUANTITIES
      N <- sum(w[])
    
    }
    ",fill = TRUE)
sink()




win.data <- list(y_aug = y_aug,
                 M = nrow(y_aug),
                 nocc = ncol(y_aug))

# Initial values
wst <- rep(1, nrow(y_aug))  	
inits <- function()list(w = rep(1, nrow(y_aug)) ,
                        p = runif(10,0,1))

# Parameters monitored
params <- c("omega", "p", "N")

# MCMC settings
ni <- 5000; nt <- 1; nb <- 500; nc <- 3

# Call JAGS from R 
out_p <- jags(win.data,inits, params, "cap_recap.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb,
            DIC = TRUE)
print(out_p)
#looking at the p's, these are the detection probs for each occasion
#Now compare the DIC's for each model. 2nd model has lower DIC and a better standard dev.

# look at diagnostics
outB <- mcmcOutput(out_p, default='N')
diagPlot(outB)

plot(outB)

# time-varying individual heterogeneity closed model ---------------------------------------------------

# Otis 1978

# called "Mth"

### ASSUMPTIONS ###

# terminology note: detections = captures

# 1. population is closed
# 2. no animals lose their marks
# 3. no false positives (animals correctly identified)
# 4. each animal has a non-zero probability of being detected
# 5. all animals have equal detection probability  [now relaxed] Now we're relaxing this assumption
# 6. detections are independent [now conditional on occasion]


sink("cap_recap.txt")
cat("
    model {
    
    # PRIORS
      
    for(j in 1:nocc){
      lp[j] ~ dlogis(0,1) #now predicting the LOGIT of p, not p. Different different dist
    }
      omega ~ dunif(0,1)
      
      tau <- 1/(sigma^2)
      sigma ~ dunif(0,10)

    # LIKELIHOOD
    for(i in 1:M){
        w[i] ~ dbern(omega)
        
        eps[i] ~ dnorm(0,tau) #higher eps is a tiger with a higher detection prob
    
        for(j in 1:nocc){
          y_aug[i,j] ~ dbern(w[i] * p[i,j])
          
          logit(p[i,j]) <- lp[j] + eps[i] #lp is no longer detection prob, it's the logit of te
          

        }#j
      }#i


    # DERIVED QUANTITIES
      N <- sum(w[])
    
    }
    ",fill = TRUE)
sink()


win.data <- list(y_aug = y_aug,
                 M = nrow(y_aug),
                 nocc = ncol(y_aug))

# Initial values
wst <- rep(1, nrow(y_aug))  	
inits <- function()list(w = rep(1, nrow(y_aug)))

# Parameters monitored
params <- c("omega", "lp", "N")

# MCMC settings
ni <- 5000; nt <- 1; nb <- 500; nc <- 3

# Call JAGS from R 
out_ph <- jags(win.data,inits, params, "cap_recap.txt", 
               n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb,
               DIC = TRUE)
#Everthing is in logit scale. use plogis(param) in order to get actual number

print(out)
print(out_p)
print(out_ph) #A little higher DIC than out_p therefore no evidence that different tigers have different detection probs. out_p is better

#Compare 3 DIC's to see which model is the best

# look at diagnostics
outB <- mcmcOutput(out_ph, default='N')
diagPlot(outB)

plot(outB)
