# load library
library(R2jags)

#See session 10 for what data looks like
# in R lm ~ means as a function of 
#in jags ~ is read as "is distributed as"

# generate data
# peregrine falcon wingspan by sex
n1 <- 60      # no. females
n2 <- 40      # no. males
mu1 <- 105    # female pop. mean (cm)
mu2 <- 77.5   # male pop. mean
sigma <- 2.5  # pop. sd (same for both sexes)
n <- n1 + n2  # total sample size
set.seed(12)
y1 <- rnorm(n1, mu1, sigma)  # female data
set.seed(12)
y2 <- rnorm(n2, mu2, sigma)  # male data
y <- c(y1,y2)                # all data
x <- rep(c(0,1),c(n1,n2))    # indicator variable for sex (male = 1)
dev.off()
boxplot(y ~ x, xlab = "Female = 0; Male = 1",
        ylab = "Wingspan (cm)")

# JAGS model
sink("ttest.txt")
cat(" 
  model {

# PRIORS
beta0 ~ dnorm(0,0.001)
beta1 ~ dnorm(0,0.001)
tau <- 1/sigma^2
sigma ~ dunif(0,10)

# LIKELIHOOD
for(i in 1:n){
y[i] ~ dnorm(mu[i],tau)
mu[i] <- beta0 + beta1*x[i]
resid[i] <- y[i] - mu[i]   # residuals

} # i
} # end of model",fill = TRUE)
sink()

# data
win.data <- list(y = y,x = x, n = n)

# Initial values
inits <- function() list(beta0 = rnorm(1), beta1 = rnorm(1), sigma = rlnorm(1))

# Parameters monitored
params <- c("beta0","beta1","resid","sigma") 

# MCMC settings
ni <- 1000; nt <- 3; nb <- 100; nc <- 3

# run model
out <- jags(win.data, inits, params, "ttest.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
print(out, dig = 1)

# see the full columns of the design matrix and linear algebra
d <- data.frame(mass = round(y,1),
           int = rep(1,length(y)),
           beta0 = round(rep(out$BUGSoutput$mean$beta0, length(y)),1),
           sex = x,
           beta1 = round(rep(out$BUGSoutput$mean$beta1, length(y)),1),
           resid = round(out$BUGSoutput$mean$resid,1))
