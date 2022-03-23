# load library
library(R2jags)

purps <- "#440154FF"
midnight <- "#414487FF"
dkteal <- "#2A788EFF"
limabean <- "#7AD151FF"
bananas <- "#FDE725FF"

ccolors <- c(purps, midnight, dkteal,limabean, bananas)


# poisson t-test data ----------------------------------------------------------

# simulate some data
# counts of hares, similar to the real dataset
# but with reduced variability
n.sites <- 20 #in each landuse
x <- gl(n = 2, k = n.sites,labels = c("grassland","arable")) # note the order!!!!!
n <- 2 * n.sites # total sample size

# simulate data from TRUE hare density as a function
# of landuse 
lambda <- exp(0.69 + 0.92*(as.numeric(x) - 1)) # need -1 b/c of how R generates levels. as. numeric converts factor into 0s and 1

# what do 0.69 and 0.92 mean? how do you interpret these values?
    # 0.69 = log of number of hares in ref category (in our case, grassland)
    # 0.92 = log of effect of being in arable compared to grassland
exp(0.69) #mean expected count of 2 hares in grassland
exp(0.92) #weird to interpret bc log stuff .92 is effect of arable of avg no of hares but on log scale

exp(.69 + .92) #avg no hares in arable

# lambda is the mean TRUE expectation, given landuse
# but, there is noise around that expectation, 
# i.e., you don't have 2 hares in every grassland site, etc.
# so, we add random error (noise), assuming a Poisson distribution
# around that expectation:
C <- rpois(n = n, lambda = lambda)     # C for Count
aggregate(C, by = list(x), FUN = mean) # observed means
boxplot(C ~ x, 
        xlab = "Landuse", 
        ylab = "Hare count",
        col = ccolors[c(4,5)]) # Add error to generate data 

# analyze using glm -------------------------------------------------------

poisson.t.test <- glm(C ~ x, family = poisson)
summary(poisson.t.test) #effect of arable are significant. ESTIMATES ARE ON LOG SCALE

# transform back to natural scale
exp(coef(poisson.t.test)[1])
exp(coef(poisson.t.test)[1] + coef(poisson.t.test)[2])

# JAGS model -----------------------------------------------------

# JAGS model - effects parameterization
sink("poisson.txt")
cat(" 
    model {
    
    # PRIORS
    beta0 ~ dnorm(0,0.001)  # intercept
    beta1 ~ dnorm(0,0.001)  # effect of arable 
    
    # LIKELIHOOD
    for(i in 1:n){
    hares[i] ~ dpois(lambda[i]) #error term built in bc lambda is both mean and var
    log(lambda[i]) <- beta0 + beta1 * landuse[i] # landuse needs to be 0s and 1s. predicting log of lambda, not lambda
    #lambda[i] <- exp(beta0 + beta1 * landuse[i] ) Alternative form
    } # i
    
    } # end of model  
    ",fill = TRUE)
sink()

win.data <- list(hares = C, 
                 landuse  = as.integer(x)-1, #make landuse a zero/ one vector
                 n  = n)

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

d <- density(o$beta0)
d2 <- density(o$beta1)

plot(d,
     xlab = "Posterior distribution",
     ylim = c(0,3),
     xlim = c(-1,1.5),
     main = "")
polygon(d, col = rgb(182,207,182, alpha = 175, maxColorValue = 255), 
        border = "black", lwd = 2)
polygon(d2, col = rgb(255,255,181, alpha = 175, maxColorValue = 255), 
        border = "black", lwd = 2) #hard to interpret. posterior dist (OF THE MEAN) on log scale

# convert to raw scale and plot
d <- density(exp(o$beta0))
d2 <- density(exp(o$beta0 + o$beta1))

plot(d,
     xlab = "Posterior distribution",
     main = "Expected hare density by landuse",
     xlim = c(0,9))
polygon(d, col = rgb(182,207,182, alpha = 175, maxColorValue = 255), 
        border = "black", lwd = 3)
polygon(d2, col = rgb(255,255,181, alpha = 175, maxColorValue = 255), 
        border = "black", lwd = 3) #converts to raw scale and plots

# JAGS model plus  -----------------------------------------------------

# JAGS model - includes bayesian p val. use fitted model to gen new dataset and then calc sum of squares. Want same amount of error btwn real data and generated data
sink("poisson.txt")
cat(" 
    model {
    
    # PRIORS
    beta0 ~ dnorm(0,0.001)  # intercept
    beta1 ~ dnorm(0,0.001)  # effect of arable 
    
    # LIKELIHOOD
    for(i in 1:n){
    hares[i] ~ dpois(lambda[i])
    log(lambda[i]) <- beta0 + beta1 * landuse[i] # landuse needs to be 0s and 1s

    # CHECK MODEL FIT
    presid[i] <- (hares[i] - lambda[i])/sqrt(lambda[i])          # Pearson residual- corrects for larger mean => larger var
    hares.new[i] ~ dpois(lambda[i])                              # new data from fitted model
    presid.new[i] <- (hares.new[i] - lambda[i])/sqrt(lambda[i])  # Pearson residual for new data

    D[i] <- presid[i]^2          # squared Pearson residuals for observed data
    D.new[i] <- presid.new[i]^2  # squared Pearson residuals for new data


    } # i

    # CHECK MODEL FIT - BAYESIAN P-VALUE
    D.sum <- sum(D[])                      # sum of squared residuals for raw data
    D.new.sum <- sum(D.new[])              # same, but for model-generated data
    Bayes.P <- step(D.new.sum/D.sum - 1)   # step(x) tests if x >= 0
    
    } # end of model  
    ",fill = TRUE)
sink()

win.data <- list(hares = C, 
                 landuse  = as.integer(x)-1,
                 n  = n)

# Initial values
inits <- function()list(beta0  = rnorm(1),
                        beta1  = rnorm(1))

# Parameters monitored
params <- c("beta0",
            "beta1",
            "Bayes.P",
            "D.sum",
            "D.new.sum",
            "hares.new") 

# MCMC settings
ni <- 3000; nt <- 1; nb <- 1000; nc <- 3

out <- jags(win.data, inits, params, "poisson.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
            working.directory = getwd())
print(out, dig = 2) #bayes p gives p value, also creates data at new datapoint

# plot parameter posteriors
o <- out$BUGSoutput$sims.list

d <- density(o$beta0)
d2 <- density(o$beta1)

plot(d,
     xlab = "Posterior distribution",
     ylim = c(0,3),
     xlim = c(-1,1.5),
     main = "",
     las = 1)
polygon(d, col = rgb(182,207,182, alpha = 175, maxColorValue = 255), 
        border = "black", lwd = 2)
polygon(d2, col = rgb(255,255,181, alpha = 175, maxColorValue = 255), 
        border = "black", lwd = 2)

# convert to raw scale and plot - what model thinks means are
d <- density(exp(o$beta0))
d2 <- density(exp(o$beta0 + o$beta1))

plot(d,
     xlab = "Posterior distribution",
     main = "Expected hare density by landuse",
     xlim = c(0,9),
     las = 1)
polygon(d, col = rgb(182,207,182, alpha = 175, maxColorValue = 255), 
        border = "black", lwd = 3)
polygon(d2, col = rgb(255,255,181, alpha = 175, maxColorValue = 255), 
        border = "black", lwd = 3)

# plot model generated new data - what model thinks data looks like
hist(o$hares.new[,x == "grassland"],
     xlab = "Hare abundance",
     col = rgb(182,207,182, alpha = 195, maxColorValue = 255),
     main = NULL,
     xlim = c(0,18),
     breaks = 10)
hist(o$hares.new[,x == "arable"],
     xlab = "Hare abundance",
     col = rgb(255,255,181, alpha = 125, maxColorValue = 255),
     main = NULL,
     add = TRUE)


# Exercise 5 in Kery 2010 -------------------------------------------------

# load real data
hares <- read.delim("hares.txt", header = TRUE)

# examine counts from the year 2000
hares <- hares[hares$year == 2000,]

# use the count with the lower mean
mean(hares$count1, na.rm = TRUE)
mean(hares$count2, na.rm = TRUE) # use count2

# visualize
boxplot(count2 ~ landuse,
        data = hares,
        ylab = "Hare count",
        xlab = "Landuse",
        col = ccolors[c(5,4)]) #real world data much more variable than generated data. would poisson work?

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
            "Bayes.P") 

# MCMC settings
ni <- 3000; nt <- 1; nb <- 1000; nc <- 3

out <- jags(win.data, inits, params, "poisson.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
            working.directory = getwd())
print(out, dig = 2) #remember this is an effects parameterization
#Dsum means model is overconfident. fix for wed. Means that there are other covariates that weren't considered that are effecting model. low bayes p val means underfit. high pval means model is overfit. fix by adding in random effect that we didn't measure


# plot parameter posteriors
o <- out$BUGSoutput$sims.list

# convert to raw scale and plot
d <- density(exp(o$beta0))
d2 <- density(exp(o$beta0 + o$beta1))

plot(d,
     xlab = "Posterior distribution",
     main = "Expected hare density by landuse",
     xlim = c(0,50),
     ylim = c(0,0.35))
polygon(d, col = rgb(255,255,181, alpha = 255, maxColorValue = 255), 
        border = "black", lwd = 3)
polygon(d2, col = rgb(182,207,182, alpha = 255, maxColorValue = 255), 
        border = "black", lwd = 3)

