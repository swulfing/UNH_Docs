# load library
library(R2jags)

purps <- "#440154FF"
midnight <- "#414487FF"
dkteal <- "#2A788EFF"
limabean <- "#7AD151FF"
bananas <- "#FDE725FF"

ccolors <- c(purps, midnight, dkteal,limabean, bananas)


# Exercise 5 in Kery 2010 -------------------------------------------------

# load real data
hares <- read.delim("hares.txt", header = TRUE)

# examine counts from the year 2000
hares <- hares[hares$year == 2000,]

# use the count with the lower mean
mean(hares$count1, na.rm = TRUE)
mean(hares$count2, na.rm = TRUE) # use count2

tapply(hares$count2,hares$landuse,FUN = mean, na.rm = TRUE)

# visualize
boxplot(count2 ~ landuse,
        data = hares,
        ylab = "Hare count",
        xlab = "Landuse",
        col = ccolors[c(5,4)]) #What the counts look like. poisitve, integers, high means

# JAGS model 
sink("poisson.txt")
cat(" 
    model {
    
    # PRIORS
    beta0 ~ dnorm(0,0.001)  
    beta1 ~ dnorm(0,0.001)  
    
    # LIKELIHOOD
    for(i in 1:n){
    hares[i] ~ dpois(lambda[i])
    log(lambda[i]) <- beta0 + beta1 * landuse[i] 

    # CHECK MODEL FIT
    presid[i] <- (hares[i] - lambda[i])/sqrt(lambda[i]) 
    hares.new[i] ~ dpois(lambda[i])   
    presid.new[i] <- (hares.new[i] - lambda[i])/sqrt(lambda[i]) 
    
    D[i] <- presid[i]^2    
    D.new[i] <- presid.new[i]^2 
    
    
    } # i
    
    # CHECK MODEL FIT - BAYESIAN P-VALUE
    D.sum <- sum(D[])  
    D.new.sum <- sum(D.new[]) 
    Bayes.P <- step(D.new.sum/D.sum - 1) 
    
    } # end of model  
    ",fill = TRUE)
sink()

win.data <- list(hares = hares$count2, 
                 landuse  = as.integer(as.factor(hares$landuse))-1,
                 n  = nrow(hares))

# Initial values
inits <- function()list(beta0  = rnorm(1),
                        beta1  = rnorm(1))

# Parameters monitored
params <- c("beta0",
            "beta1",
            "D.sum",
            "D.new.sum",
            "hares.new",
            "Bayes.P") 

# MCMC settings
ni <- 3000; nt <- 1; nb <- 1000; nc <- 3

out <- jags(win.data, inits, params, "poisson.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
            working.directory = getwd())
print(out, dig = 2) #arable is beta 0, grassland is beta 1

#d.sum is data mean of errors. d.new.sum is simulated data's mean of errors. This would be a good thing to put in methods and report into results

# plot parameter posteriors
o <- out$BUGSoutput$sims.list

# convert to natural scale and plot
d <- density(exp(o$beta0))
d2 <- density(exp(o$beta0 + o$beta1))

plot(d,
     xlab = "Posterior distribution", #of the means. modeled data
     main = "Expected hare abundance by landuse",
     xlim = c(0,45),
     ylim = c(0,0.35))
polygon(d, col = rgb(255,255,181, alpha = 255, maxColorValue = 255), 
        border = "black", lwd = 3)
polygon(d2, col = rgb(182,207,182, alpha = 255, maxColorValue = 255), 
        border = "black", lwd = 3)

# plot actual predictions of hare abundance
d <- o$hares.new[,hares$landuse == "arable"]
d2 <- o$hares.new[,hares$landuse == "grass"]

par(mfrow = (c(1,2)))
hist(d2, col = rgb(182,207,182, alpha = 255, maxColorValue = 255),
     breaks = 50,
     xlab = "Predicted number of hares",
     main = "Grassland")
hist(d, col = rgb(255,255,181, alpha = 255, maxColorValue = 255),
     breaks = 50,
     xlab = "Predicted number of hares",
     main = "Arable")
dev.off() #spread is a lot bigger in predictions as opposed to posterior mena dist. makes sense bc poly graph is mapping mean probabilities. Data (hist) will inherently have more spread than the mean
#These illistrate difference btwn credible interval (poly graph, 95% sure mean is btwn two numbers) and prediction interval (predicts data. conficence interval is the analog to ci in frequentist)
#prediction intervals contain 95% of the data. prediction interval is always larger

# plots of real data
par(mfrow = (c(1,2)))
hist(hares$count2[hares$landuse == "grass"], 
     col = rgb(182,207,182, alpha = 255, maxColorValue = 255),
     breaks = 50,
     xlab = "Observed number of hares",
     main = "Grassland")
hist(hares$count2[hares$landuse == "arable"], 
     col = rgb(255,255,181, alpha = 255, maxColorValue = 255),
     breaks = 50,
     xlab = "Observed number of hares",
     main = "Arable")
dev.off()


par(mfrow = (c(2,2)))
hist(d2, col = rgb(182,207,182, alpha = 255, maxColorValue = 255),
     breaks = 50,
     xlab = "Predicted number of hares",
     main = "Grassland")
hist(d, col = rgb(255,255,181, alpha = 255, maxColorValue = 255),
     breaks = 50,
     xlab = "Predicted number of hares",
     main = "Arable")
hist(hares$count2[hares$landuse == "grass"], 
     col = rgb(182,207,182, alpha = 255, maxColorValue = 255),
     breaks = 50,
     xlab = "Observed number of hares",
     main = "Grassland")
hist(hares$count2[hares$landuse == "arable"], 
     col = rgb(255,255,181, alpha = 255, maxColorValue = 255),
     breaks = 50,
     xlab = "Observed number of hares",
     main = "Arable")

dev.off() #shows why our bayesian p value is terrible

#to fix, choose different model or transform data

#issues:
#lots of zeros i.e. zero inflation
#data doesn't look normal or like any dist. smaller sample size. even at large datasizes with lots of zeros model will still be bad
#Lots of high values i.e. overdispersion


# Exercise 5 in Kery 2010 OD -------------------------------------------------

# load real data
hares <- read.delim("hares.txt", header = TRUE)

# examine counts from the year 2000
hares <- hares[hares$year == 2000,]

# use the count with the lower mean
mean(hares$count1, na.rm = TRUE)
mean(hares$count2, na.rm = TRUE) # use count2
dev.off()
# visualize
boxplot(count2 ~ landuse,
        data = hares,
        ylab = "Hare count",
        xlab = "Landuse",
        col = ccolors[c(5,4)])

# JAGS model 
sink("poisson.txt")
cat(" 
    model {
    
    # PRIORS
    beta0 ~ dnorm(0,0.001)  
    beta1 ~ dnorm(0,0.001)  
    tau <- 1/sigma^2
    sigma ~ dunif(0,10)
    
    # LIKELIHOOD
    for(i in 1:n){
    hares[i] ~ dpois(lambda[i])
    log(lambda[i]) <- beta0 + beta1 * landuse[i] + eps[i]

    eps[i] ~ dnorm(0,tau) # extra site-level variance not captured by the covariates. new pit
    
    # CHECK MODEL FIT
    presid[i] <- (hares[i] - lambda[i])/sqrt(lambda[i]) 
    hares.new[i] ~ dpois(lambda[i])   
    presid.new[i] <- (hares.new[i] - lambda[i])/sqrt(lambda[i]) 
    
    D[i] <- presid[i]^2    
    D.new[i] <- presid.new[i]^2 
    
    
    } # i
    
    # CHECK MODEL FIT - BAYESIAN P-VALUE
    D.sum <- sum(D[])  
    D.new.sum <- sum(D.new[]) 
    Bayes.P <- step(D.new.sum/D.sum - 1) 
    
    } # end of model  
    ",fill = TRUE)
sink()

win.data <- list(hares = hares$count2, 
                 landuse  = as.integer(as.factor(hares$landuse))-1,
                 n  = nrow(hares))

# Initial values
inits <- function()list(beta0  = rnorm(1),
                        beta1  = rnorm(1),
                        sigma = rlnorm(1))

# Parameters monitored
params <- c("beta0",
            "beta1",
            "D.sum",
            "D.new.sum",
            "Bayes.P",
            "hares.new",
            "eps",
            "sigma") 

# MCMC settings
ni <- 8000; nt <- 1; nb <- 1000; nc <- 3

out <- jags(win.data, inits, params, "poisson.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
            working.directory = getwd())
print(out, dig = 2)

# plot parameter posteriors. How we have a bayes p val of .5 which is the best. betas are actually similar. different estimated eps for each site
o <- out$BUGSoutput$sims.list

# convert to raw scale and plot
d <- density(exp(o$beta0))
d2 <- density(exp(o$beta0 + o$beta1))

plot(d,
     xlab = "Posterior distribution",
     main = "Expected hare density by landuse",
     xlim = c(0,33),
     ylim = c(0,0.15))
polygon(d, col = rgb(255,255,181, alpha = 255, maxColorValue = 255), 
        border = "black", lwd = 3)
polygon(d2, col = rgb(182,207,182, alpha = 155, maxColorValue = 255), 
        border = "black", lwd = 3) #means are a lot more diffused. model is more uncertain (this is good)

# plot actual predictions of hare abundance
d <- o$hares.new[,hares$landuse == "arable"]
d2 <- o$hares.new[,hares$landuse == "grass"]
d <- d[d<150]
d2 <- d2[d2 < 150]
par(mfrow = (c(1,2)))
hist(d2, col = rgb(182,207,182, alpha = 255, maxColorValue = 255),
     breaks = 50,
     xlim = c(0,80),
     xlab = "Predicted number of hares",
     main = "Grassland")
hist(d, col = rgb(255,255,181, alpha = 255, maxColorValue = 255),
     breaks = 100,
     xlim = c(0,80),
     xlab = "Predicted number of hares",
     main = "Arable")
dev.off()

par(mfrow = (c(2,2)))
hist(d2, col = rgb(182,207,182, alpha = 255, maxColorValue = 255),
     breaks = 50,
     xlim = c(0,60),
     xlab = "Predicted number of hares",
     main = "Grassland")
hist(d, col = rgb(255,255,181, alpha = 255, maxColorValue = 255),
     breaks = 100,
     xlim = c(0,80),
     xlab = "Predicted number of hares",
     main = "Arable")
hist(hares$count2[hares$landuse == "grass"], 
     col = rgb(182,207,182, alpha = 255, maxColorValue = 255),
     breaks = 50,
     xlab = "Observed number of hares",
     main = "Grassland")
hist(hares$count2[hares$landuse == "arable"], 
     col = rgb(255,255,181, alpha = 255, maxColorValue = 255),
     breaks = 50,
     xlab = "Observed number of hares",
     main = "Arable")

dev.off() #this one site in arable is causing a small bump in the model. 
#Can't use this model for a new site because the error term is not based on anything


# zero-inflated Poisson data ---------------------------------------------------

# simulate data
psi <- 0.7 # probability that site is suitable
n.site <- 40 # number of sites in each land use category
x <- gl(n = 2, k = n.site, labels = c("grassland","arable"))

# generate suitable and non-suitable sites
w <- rbinom(n = 2*n.site, size = 1, prob = psi)#essentially a coin flip. bernoulli is special case of binomial
w

# linear predictor - counts depend upon land use as before
lambda <- exp(0.69 + 0.92*(as.numeric(x)-1)) # means of 2 and 5 in grass and arable

# now simulate zero-inflated count data
C <- rpois(n = 2*n.site, lambda = w*lambda) #when we generate data, we're multiplying by the bernoulli result
par(mfrow = (c(2,2)))
hist(C, main = "All counts",xlab = "No. hares", col = midnight)
hist(C[x == "grassland"], main = "Grassland counts",xlab = "No. hares", col = limabean)
hist(C[x == "arable"], main = "Arable counts",xlab = "No. hares", col = bananas)

# inspect dataset
cc <- data.frame(x,w,C)
cc


# regular Poisson JAGS analysis -----------------------------------------------------------

# JAGS model that DOES NOT have zero-inflation
sink("poisson.txt")
cat(" 
    model {
    
    # PRIORS
    beta0 ~ dnorm(0,0.001)  
    beta1 ~ dnorm(0,0.001)  
    
    # LIKELIHOOD
    for(i in 1:n){
    hares[i] ~ dpois(lambda[i])
    log(lambda[i]) <- beta0 + beta1 * landuse[i] 
    
    # CHECK MODEL FIT
    presid[i] <- (hares[i] - lambda[i])/sqrt(lambda[i]) 
    hares.new[i] ~ dpois(lambda[i])   
    presid.new[i] <- (hares.new[i] - lambda[i])/sqrt(lambda[i]) 
    
    D[i] <- presid[i]^2    
    D.new[i] <- presid.new[i]^2 

    } # i
    
    # CHECK MODEL FIT - BAYESIAN P-VALUE
    D.sum <- sum(D[])  
    D.new.sum <- sum(D.new[]) 
    Bayes.P <- step(D.new.sum/D.sum - 1) 
    
    } # end of model  
    ",fill = TRUE)
sink()

win.data <- list(hares = C, 
                 landuse  = as.numeric(x)-1,
                 n  = length(C))

# Initial values
inits <- function()list(beta0  = rnorm(1),
                        beta1  = rnorm(1))

# Parameters monitored
params <- c("beta0",
            "beta1",
            "Bayes.P") 

# MCMC settings
ni <- 10000; nt <- 1; nb <- 1000; nc <- 3

out <- jags(win.data, inits, params, "poisson.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
            working.directory = getwd())
print(out, dig = 2)
#Terrible bayes p val


# plot parameter posteriors
o <- out$BUGSoutput$sims.list

# convert to raw scale and plot
d <- density(exp(o$beta0))
d2 <- density(exp(o$beta0 + o$beta1))

par(mfrow = (c(1,1)))
plot(d,
     xlab = "Posterior distribution",
     main = "Expected hare abundance at suitable sites",
     xlim = c(0,6))
polygon(d, col = rgb(182,207,182, alpha = 255, maxColorValue = 255), 
        border = "black", lwd = 3)
polygon(d2, col = rgb(255,255,181, alpha = 255, maxColorValue = 255), 
        border = "black", lwd = 3)
abline(v = exp(0.69), 
       lwd = 4, lty = 2)
abline(v = exp(0.69 + 0.92), 
       lwd = 4, lty = 3)

#Dashed lines are actual means. model is underestimating hares 

# zero-inflated Poisson JAGS analysis -----------------------------------------------------------

# JAGS model WITH zero-inflation
sink("poisson.txt")
cat(" 
    model {
    
    # PRIORS
    beta0 ~ dnorm(0,0.001)  
    beta1 ~ dnorm(0,0.001)  
    psi ~ dunif(0,1)
    
    # LIKELIHOOD
    for(i in 1:n){

    w[i] ~ dbern(psi) #Adding a term
    hares[i] ~ dpois(w[i]*lambda[i])
    log(lambda[i]) <- beta0 + beta1 * landuse[i] 
    
    # CHECK MODEL FIT
    presid[i] <- (hares[i] - w[i]*lambda[i])/sqrt(w[i]*lambda[i] + 0.0001) 
    hares.new[i] ~ dpois(w[i]*lambda[i])   
    presid.new[i] <- (hares.new[i] - w[i]*lambda[i])/sqrt(w[i]*lambda[i]+ 0.0001) 
    
    D[i] <- presid[i]^2    
    D.new[i] <- presid.new[i]^2 

    } # i
    
    # CHECK MODEL FIT - BAYESIAN P-VALUE
    D.sum <- sum(D[])  
    D.new.sum <- sum(D.new[]) 
    Bayes.P <- step(D.new.sum/D.sum - 1) 
    
    } # end of model  
    ",fill = TRUE)
sink()

win.data <- list(hares = C, 
                 landuse  = as.numeric(x)-1,
                 n  = length(C))

# Initial values
inits <- function()list(beta0  = rnorm(1),
                        beta1  = rnorm(1),
                        w = rep(1,length(C))) # note: need "good" initial values for w

# Parameters monitored
params <- c("beta0",
            "beta1",
            "psi",
            "Bayes.P") 

# MCMC settings
ni <- 40000; nt <- 1; nb <- 1000; nc <- 3

out <- jags(win.data, inits, params, "poisson.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
            working.directory = getwd())
print(out, dig = 2) #Way better p val

# plot parameter posteriors
o <- out$BUGSoutput$sims.list

# convert to raw scale and plot
d <- density(exp(o$beta0))
d2 <- density(exp(o$beta0 + o$beta1))

plot(d,
     xlab = "Posterior distribution",
     main = "Expected hare abundance at suitable sites",
     xlim = c(0,7))
polygon(d, col = rgb(182,207,182, alpha = 255, maxColorValue = 255), 
        border = "black", lwd = 3)
polygon(d2, col = rgb(255,255,181, alpha = 255, maxColorValue = 255), 
        border = "black", lwd = 3)
abline(v = exp(0.69), 
       lwd = 4, lty = 2)
abline(v = exp(0.69 + 0.92), 
       lwd = 4, lty = 3)
#This is a lot better than last plot
#You should almost never use this because you should know your data enough to tell you about suitability that you can include as a covariate


# offset ------------------------------------------------------------------

n.site <- 20
A <- runif(n = 2*n.site, 2,5) # area ranges from 2 to 5 km2
x <- gl(n = 2, k = n.site, labels = c("grassland","arable"))
linear.predictor <- log(A) + 0.69 + 0.92*(as.integer(x)-1)
lambda <- exp(linear.predictor)
C <- rpois(n = 2*n.site, lambda = lambda)


# JAGS model 
sink("poisson.txt")
cat(" 
    model {
    
    # PRIORS
    beta0 ~ dnorm(0,0.001)  
    beta1 ~ dnorm(0,0.001)  
    
    # LIKELIHOOD
    for(i in 1:n){
    hares[i] ~ dpois(lambda[i])
    log(lambda[i]) <- log(A[i]) + beta0 + beta1 * landuse[i] 
    } # i
    } # end of model  
    ",fill = TRUE)
sink()

win.data <- list(hares = C, 
                 landuse  = as.integer(as.factor(x))-1,
                 n  = n.site*2,
                 A = A)

# Initial values
inits <- function()list(beta0  = rnorm(1),
                        beta1  = rnorm(1))

# Parameters monitored
params <- c("beta0",
            "beta1") 

# MCMC settings
ni <- 3000; nt <- 1; nb <- 1000; nc <- 3

out <- jags(win.data, inits, params, "poisson.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
            working.directory = getwd())
print(out, dig = 2)

# plot parameter posteriors
o <- out$BUGSoutput$sims.list

# convert to raw scale and plot
d <- density(exp(o$beta0))
d2 <- density(exp(o$beta0 + o$beta1))

plot(d,
     xlab = "Posterior distribution",
     main = "Expected hare abundance by landuse",
     xlim = c(0,8))
polygon(d, col = rgb(255,255,181, alpha = 255, maxColorValue = 255), 
        border = "black", lwd = 3)
polygon(d2, col = rgb(182,207,182, alpha = 255, maxColorValue = 255), 
        border = "black", lwd = 3)


# failure to account for offset -------------------------------------------

# JAGS model 
sink("poisson.txt")
cat(" 
    model {
    
    # PRIORS
    beta0 ~ dnorm(0,0.001)  
    beta1 ~ dnorm(0,0.001)  
    
    # LIKELIHOOD
    for(i in 1:n){
    hares[i] ~ dpois(lambda[i])
    log(lambda[i]) <- beta0 + beta1 * landuse[i] 
    } # i
    } # end of model  
    ",fill = TRUE)
sink()

win.data <- list(hares = C, 
                 landuse  = as.integer(as.factor(x))-1,
                 n  = n.site*2,
                 A = A)

# Initial values
inits <- function()list(beta0  = rnorm(1),
                        beta1  = rnorm(1))

# Parameters monitored
params <- c("beta0",
            "beta1") 

# MCMC settings
ni <- 3000; nt <- 1; nb <- 1000; nc <- 3

out <- jags(win.data, inits, params, "poisson.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
            working.directory = getwd())
print(out, dig = 2)

# plot parameter posteriors
o <- out$BUGSoutput$sims.list

# convert to raw scale and plot
d <- density(exp(o$beta0))
d2 <- density(exp(o$beta0 + o$beta1))

plot(d,
     xlab = "Posterior distribution",
     main = "Expected hare abundance by landuse",
     xlim = c(0,22))
polygon(d, col = rgb(255,255,181, alpha = 255, maxColorValue = 255), 
        border = "black", lwd = 3)
polygon(d2, col = rgb(182,207,182, alpha = 255, maxColorValue = 255), 
        border = "black", lwd = 3)


