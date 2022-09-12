# load library
library(R2jags)

purps <- "#440154FF"
midnight <- "#414487FF"
dkteal <- "#2A788EFF"
limabean <- "#7AD151FF"
bananas <- "#FDE725FF"

ccolors <- c(purps, midnight, dkteal,limabean, bananas)

# simlulate data ----------------------------------------------------------

n.groups <- 56   # number of populations
n.sample <- 10   # samples of population
n <- n.groups * n.sample # total sample size

# generate populations as factors, numerically defined (useful for JAGS)
pop <- gl(n = n.groups, # number of factors
          k = n.sample) # number of samples per factor


# body length (cm)
original.length <- runif(n,45,70)
hist(original.length,
     main = "",
     xlab = "Snake length (cm)")

# standardize length (will help JAGS converge)-how to do it manually. If we altered bin width, it would be identical dist
mn <- mean(original.length)
sd <- sd(original.length)
length <- (original.length - mn)/sd
hist(length,
     main = "",
     xlab = "Standardized length")

# build a design matrix without intercept
Xmat <- model.matrix(~pop*length-1-length) # builds means parametization mtx
print(Xmat[1:21,],dig = 2)
write.csv(Xmat,"Xmat.csv")

# generate parameter values for the simulation
# do so by drawing them from hyperdistributions
# we need a distribution for the intercepts, and
# one for the means
intercept.mean <- 230   # the mean mass when length = 0 (BUT WHAT DOES THAT MEAN?!) Because of standardizing, this is actually mean mass at mean length
intercept.sd   <- 20    
slope.mean     <- 60   # the mean positive association b/t length and mass
slope.sd       <- 30

# now generate all the parameters from these hyperdistributions
intercept.effects <- rnorm(n = n.groups, 
                           mean = intercept.mean, 
                           sd = intercept.sd)
slope.effects <- rnorm(n = n.groups, 
                           mean = slope.mean, 
                           sd = slope.sd)
all.effects <- c(intercept.effects,slope.effects)

# Visualize intercepts
plot(intercept.effects, xlab = "Intercept number", ylab = "Value", pch = 19)
abline(h = intercept.mean, lty = 2, lwd = 3)
abline(h = intercept.mean + intercept.sd, lty = 2, lwd = 3, col = "gray")
abline(h = intercept.mean - intercept.sd, lty = 2, lwd = 3, col = "gray") #56 total intercepts. Black is mean intercept, gray is +- sd

hist(intercept.effects, main = "Histogram of random intercepts",
     xlab = "Value", col = midnight)

# Visualize slopes
plot(slope.effects, xlab = "Slope number", ylab = "Value", pch = 19)#Intercept version of ^
abline(h = slope.mean, lty = 2, lwd = 3)
abline(h = slope.mean + intercept.sd, lty = 2, lwd = 3, col = "gray")
abline(h = slope.mean - intercept.sd, lty = 2, lwd = 3, col = "gray")

hist(slope.effects, main = "Histogram of random slopes",
     xlab = "Value", col = midnight)


# we use matrix multiplication to generate the data set. take matrix model and muptiply by mtx of params to get linear predictor
lin.pred <- Xmat[,] %*% all.effects     # value of liinear predictor (i.e., the mean)
eps <- rnorm(n = n, mean = 0, sd = 30)  # residual error around the mean
mass <- lin.pred + eps # simulated dataset: mean response + error

# Visualize
hist(mass, col = dkteal)

# plot first population
plot(mass[pop == 1] ~ length[pop == 1], pch = 19, cex = 1.5, col = ccolors[1])
abline(a = intercept.effects[1], b = slope.effects[1], lwd = 4, 
       col = ccolors[1])

# plot first nine populations
par(mfrow = c(3,3))
cccolors <- rep(ccolors, length.out = 16)
cccolors <- cccolors
ccolors2 <- c(dkteal,limabean, bananas,purps,midnight)
cccolors2 <- rep(ccolors2, length.out = 16)
ccolors3 <- c(limabean, bananas,purps,midnight,dkteal)
cccolors3 <- rep(ccolors3, length.out = 16)

for(i in 1:9){#plotting each pop. each has different slope and intercept
    plot(mass[pop == i] ~ length[pop == i], 
         pch = 21, bg = cccolors3[i], cex = 2.5, cex.lab = 1.45, col = cccolors[i],
         main = paste0("Population ",i),
         xlab = "Length (standardized)",
         ylab = "Snake mass (g)",
         ylim = c(min(mass),max(mass)))
    rect(par("usr")[1], par("usr")[3],
         par("usr")[2], par("usr")[4],
         col = cccolors2[i]) 
    abline(a = intercept.effects[i], b = slope.effects[i], lwd = 6, 
           col = cccolors[i])   
    points(mass[pop == i] ~ length[pop == i], 
           pch = 21, bg = cccolors3[i], lwd = 3, cex = 2.5, col = cccolors[i])
}
dev.off()

# random intercepts model using JAGS -----------------------------------------------------

# JAGS model - random intercepts
sink("lme.model1.txt")
cat(" 
    model {
    
    # PRIORS
    for(i in 1:ngroups){
    alpha[i] ~ dnorm(mu.int,tau.int)  # intercepts
    }
    
    # hyperprior for intercepts
    mu.int ~ dnorm(0,0.001)
    tau.int <- 1/sigma.int^2
    sigma.int ~ dunif(0,100) 
    
    # slope (one common slope for all pops)
    beta ~ dnorm(0,0.001)
    
    # prior for variance
    tau <- 1/sigma^2
    sigma ~ dunif(0,100)
    
    # LIKELIHOOD
    for(i in 1:n){
    
    mass[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha[pop[i]] + beta *length[i]

    } # i

     } # end of model  
    ",fill = TRUE)
sink()

win.data <- list(mass = as.numeric(mass), # convert mass from matrix into a vector
                 pop  = pop,
                 length = length,
                 n  = n,
                 ngroups = max(as.numeric(pop)))

# Initial values
inits <- function()list(mu.int = rnorm(1),
                        beta  = rnorm(1),
                        sigma.int = rlnorm(1),
                        sigma = rlnorm(1))

# Parameters monitored
params <- c("alpha",
            "beta",
            "mu.int",
            "sigma",
            "sigma.int"
) 

# MCMC settings
ni <- 5000; nt <- 3; nb <- 1000; nc <- 3

out <- jags(win.data, inits, params, "lme.model1.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
            working.directory = getwd())
print(out, dig = 2)


# compare with true values
o <- out$BUGSoutput$mean
o$mu.int; o$sigma.int; o$beta;  o$sigma #Model thinks there's more variation than there really is. This is because model didn't account for different slopes
intercept.mean; intercept.sd; slope.mean; sd(eps)

# plot model alphas vs. true alphas
plot(out$BUGSoutput$mean$alpha ~ intercept.effects,
     ylab = "Model parameter posterior means",
     xlab = "True values")
abline(1,1, lwd = 3)

# random intercepts and slopes model using JAGS -----------------------------------------------------

# JAGS model - random intercepts
sink("lme.model2.txt")
cat(" 
    model {
    
    # PRIORS
    for(i in 1:ngroups){
    alpha[i] ~ dnorm(mu.int,tau.int)  # intercepts
    beta[i] ~ dnorm(mu.slope,tau.slope) # slopes
    }
    
    # hyperpriors for intercepts and slopes
    mu.int ~ dnorm(0,0.001)
    tau.int <- 1/sigma.int^2
    sigma.int ~ dunif(0,100) 
    mu.slope ~ dnorm(0,0.001)
    tau.slope <- 1/sigma.slope^2
    sigma.slope ~ dunif(0,100) 
    
    
    # prior for variance
    tau <- 1/sigma^2
    sigma ~ dunif(0,100)
    
    # LIKELIHOOD
    for(i in 1:n){

    mass[i] ~ dnorm(mu[i], tau)
    mu[i] <-  alpha[pop[i]] + beta[pop[i]] * length[i]

    } # i

     } # end of model  
    ",fill = TRUE)
sink()

win.data <- list(mass = as.numeric(mass), # convert mass from matrix into a vector
                 pop  = pop,
                 length = length,
                 n  = n,
                 ngroups = max(as.numeric(pop)))

# Initial values
inits <- function()list(mu.int = rnorm(1),
                        sigma.int = rlnorm(1),
                        mu.slope = rnorm(1),
                        sigma.slope = rlnorm(1),
                        sigma = rlnorm(1))

# Parameters monitored
params <- c("alpha",
            "beta",
            "mu.slope",
            "sigma.slope",
            "mu.int",
            "sigma",
            "sigma.int"
) 

# MCMC settings
ni <- 5000; nt <- 3; nb <- 1000; nc <- 3

out2 <- jags(win.data, inits, params, "lme.model2.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
            working.directory = getwd())
print(out2, dig = 2)


# compare with true values
o <- out2$BUGSoutput$mean
o$mu.int; o$sigma.int; o$mu.slope; o$sigma.slope; o$sigma
intercept.mean; intercept.sd; slope.mean; slope.sd;  sd(eps) #Alot better error estimation



# correlated random intercepts and slopes model ---------------------------


#looking to see if there's a correlation between random intercept and random slope
# JAGS model - random intercepts
sink("lme.model3.txt")
cat(" 
    model {
    
    # PRIORS
    for(i in 1:ngroups){
    alpha[i] <-  B[i,1]
    beta[i]  <-  B[i,2]
    B[i, 1:2] ~ dmnorm(B.hat[i,], Tau.B[,])
    B.hat[i,1] <- mu.int
    B.hat[i,2] <- mu.slope
    }
    
    # hyperpriors for intercepts and slopes

    mu.int ~ dnorm(0,0.001)
    mu.slope ~ dnorm(0,0.001)
    
    Tau.B[1:2, 1:2] <- inverse(Sigma.B[,])
    
    Sigma.B[1,1] <- sigma.int^2   
    sigma.int ~ dunif(0,100)
    Sigma.B[2,2] <- sigma.slope^2
    sigma.slope ~ dunif(0,100)
    
    Sigma.B[1,2] <- rho * sigma.int*sigma.slope
    Sigma.B[2,1] <- Sigma.B[1,2]
    covariance <- Sigma.B[1,2]
    rho ~ dunif(-1,1)
    
    
    # prior for variance
    tau <- 1/sigma^2
    sigma ~ dunif(0,100)
    
    # LIKELIHOOD
    for(i in 1:n){
    mass[i] ~ dnorm(mu[i],tau)
    mu[i] <- alpha[pop[i]] + beta[pop[i]] * length[i]
    } # i

     } # end of model  
    ",fill = TRUE)
sink()

win.data <- list(mass = as.numeric(mass), # convert mass from matrix into a vector
                 pop  = pop,
                 length = length,
                 n  = n,
                 ngroups = max(as.numeric(pop)))

# Initial values
inits <- function()list(mu.int = rnorm(1),
                        sigma.int = rlnorm(1),
                        mu.slope = rnorm(1),
                        sigma.slope = rlnorm(1),
                        rho = runif(1,-1,1),
                        sigma = rlnorm(1))

# Parameters monitored
params <- c(#"alpha",
            #"beta",
            "mu.slope",
            "sigma.slope",
            "mu.int",
            "covariance",
            "rho", #rho somehow tells you if they're correlated?
            "sigma",
            "sigma.int"
) 

# MCMC settings
ni <- 5000; nt <- 3; nb <- 1000; nc <- 3

out2 <- jags(win.data, inits, params, "lme.model3.txt", 
             n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
             working.directory = getwd())
print(out2, dig = 2)
out2$BUGSoutput$mean$covariance 

# compare with true values
o <- out2$BUGSoutput$mean
o$mu.int; o$sigma.int; o$mu.slope; o$sigma.slope; o$sigma
intercept.mean; intercept.sd; slope.mean; slope.sd;  sd(eps)


