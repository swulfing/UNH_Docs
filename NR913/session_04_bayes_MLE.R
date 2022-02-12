


# simple linear model simulation -----------------------------------------------------

beta0 <- 9.5
beta1 <- 3.5
krill <- runif(25,0,10) # does not need to be runif. Our ind. var

# deterministic "true" model - no error here, not a statistical model yet
true_penguins <- beta0 + beta1*krill 

dev.off()
plot(true_penguins ~ krill, 
     ylab = "No. penguins", 
     xlab = "Krill density (indiv/m2)")

# add error
set.seed(71) #generating our dataset. Weiting it as error with normal dist with mean 0, sd = 0
penguins_data <- true_penguins + rnorm(n = 25,mean = 0, sd = 5)

set.seed(71)
penguins_data2 <- rnorm(n = 25,mean = beta0 + beta1*krill, sd = 5)

# why are the above two datasets the same?

plot(penguins_data ~ krill, 
     ylab = "Penguin density (indiv/km2)", 
     xlab = "Krill density (indiv/m2)",
     pch = 20)

# linear model
mod <- lm(penguins_data ~ krill)
summary(mod)

plot(penguins_data ~ krill, 
     ylab = "No. penguins (indiv/km2)", 
     xlab = "Krill density (indiv/m2)",
     pch = 20)
abline(a =beta0, b = beta1, lwd = 3)
abline(a =mod$coefficients[1], 
       b = mod$coefficients[2],
       lwd = 3,lty = 2, col = "blue")


# the dnorm etc functions -------------------------------------------------

# what does the dbinom, dnorm, dpoisson function do?
# gives the density of the function
# for probability mass functions, pmfs, this is a probability
# for probability density functions, pdfs, this is a probability density, not a probability

# poisson distribution
d <- rpois(10000,3.5)
mean(d)
var(d)

hist(d)

dpois(x = 3, lambda = 3) #for discrete, will give us a probability. for continuous, gives you the probability dist
dpois(x = 2, lambda = 3)
dpois(x = 1, lambda = 3)
p <- dpois(x = 0:20, lambda = 3)

barplot(p, names.arg = 0:20,
        ylab = "probability")

sum(p) #not really 1 becuase we cut off at 20 but r is going to round

# binomial distribution
rbinom(n = 20, 
       size = 10,
       prob = .2) #n is number of measurements. size is trials within each meausrement. When size = 1 is bernoulli

dbinom(x = 2,
       size = 10,
       prob = 0.2) #what is the probability of x successes in size flips with a probability of prob success

b <- dbinom(x = 0:10,
            size = 10,
            prob = 0.2)

barplot(b,
        names.arg = 0:10,
        ylab = "probability",
        xlab = "No. successes out of 10",
        main = "p = 0.2")
sum(b) #variance of a binomial dist is always the same = npq where n is number of attempts in a trial
#mean is probabilit (p). Also having size = 10 sets upper limit of probability

# normal distribution-continuous
r <- rnorm(n = 100,
      mean = 5,
      sd = 3)

hist(r)

dnorm(x = 5,
      mean = 5,
      sd = 3) #This is not a probability. This is a probability DENSITY. a continuous dist, not disscrete

barplot(dnorm(seq(1,10, by = 0.01), 
              mean = 5,
              sd = 3))

barplot(dnorm(seq(1,10, by = 0.01), 
              mean = 5,
              sd = 0.1),
        names.arg = seq(0,10, by = 0.01))

# likelihood plot ---------------------------------------------------------

d1 <- dbinom(x = 20, #tadpole example = y. Liklihood you see 20 tadpoles
       size = 50, #tadpole example = n
       prob = 0.4) #Does binomial calculation where
#11.4% chance you will see 20 if detection probability is .4

d2 <- dbinom(40,50,0.4) #liklihood you see 40 tadpoles

liklihood_.4 <- d1*d2


d3 <- dbinom(x = 20, #tadpole example = y. Liklihood you see 20 tadpoles
             size = 50, #tadpole example = n
             prob = 0.5) #Does binomial calculation where
#11.4% chance you will see 20 if detection probability is .4

d4 <- dbinom(40,50,0.5) #liklihood you see 40 tadpoles

liklihood_.5 <- d3*d4 #More likely becuse 40 is so high, so higher detection liklihood will result in higher prob

# plot the likelihood
plot(dbinom(20,50,seq(0.1,1,by=0.05)) ~ seq(0.1,1,by=0.05), #seq just means we're plotting  abunch of different thetas
     ylab = "Probability of seeing 20 tadpoles",
     xlab = "Theta (Detection probability)") #Here, the peak is the max liklihood


# max likelihood by optim & glm -------------------------------------------------


# data
r <- 20 # y no. of tadpoles observed
N <- 50 # trye no. of tadpoles

# log-likelihood function
# is logged and negative for computational efficiency
nll <- function(p) -dbinom(r, size = N, prob = p,
                           log = TRUE)

# minimize the function above for the observed data
# and return the Maximum Likelihood Estimate (MLE)
fit <- optim(par = 0.5, fn = nll, method = "BFGS") # par is starting parameter and then vary p over that
fit #Will get some errors because only one datapoint but will still run. par = 0.4 is our maximum likilhood

# Use a glm to find the parameter 
# this is on the logit scale (logit link). cbind refers to 20 successes, 30 failures
fm <- glm(cbind(20,30) ~ 1, family = binomial) #lm and glm run this optim algorithm in background
summary(fm) #will give same liklihood. but it's -0.4. because y~dbimom(p,n) w/ p prob of success, n no. trials
#systematic part is logit(p) = B0 + B1x makes sure p is actually btwn 0 and 1. 
#So taking summary of glm gives logit of p. So you need plogis (below) to transform back to p

# inverse logit function to get the probability
plogis(fm$coefficients[1])




# jags model --------------------------------------------------------------

library(R2jags)

# Save JAGS description of the model to working directory. 
# specify a mean and precision. Precision is 1/variance. Order does not matter.
sink("penguins.txt")
cat("
    model {
    
    # Priors
    beta0 ~ dnorm(0,0.01)		# precision inverse of variance
    beta1 ~ dnorm(0,0.01)
    precision <- 1 / variance	
    variance <- sigma^2
    sigma ~ dunif(0,15)
    
    
    # Likelihood
    for(i in 1:nobs){
    penguins[i] ~ dnorm(mew[i], precision)
    
    mew[i] <- beta0 + beta1 * krill[i]
    
    } # i loop
    
    } # end of the model
    ",fill=TRUE)
sink()

# Bundle data
win.data <- list(penguins = penguins_data, 
                 krill = krill, 
                 nobs = length(penguins_data))

# Function to generate starting values aka initial values
inits <- function()list(beta0 = rnorm(1), 
                        sigma = runif(1, 0, 15))

# Parameters to be monitored (= to estimate)
params <- c("beta0", 
            "beta1", 
            "sigma")

# MCMC settings
nc <- 3					# Number of chains
ni <- 1000			# Number of draws from posterior (for each chain)
nb <- 1					# Number of draws to discard as burn-in
nt <- 1					# Thinning rate

# prepare data
out <- jags(win.data, inits, params, "penguins.txt", n.chains = nc, 
            n.thin = nt, n.iter = ni, n.burnin = nb, working.directory = getwd())

print(out, dig =2)

# explore the output

# plot
plot(penguins_data ~ krill, 
     ylab = "No. penguins (indiv/km2)", 
     xlab = "Krill density (indiv/m2)",
     pch = 20)
abline(a =beta0, b = beta1, lwd = 3)
abline(a =mod$coefficients[1], 
       b = mod$coefficients[2],
       lwd = 4,lty = 2, col = "blue")
abline(a =out$BUGSoutput$mean$beta0, 
       b = out$BUGSoutput$mean$beta1,
       lwd = 4,lty = 3, col = "red")

# look at convergence
traceplot(out)

