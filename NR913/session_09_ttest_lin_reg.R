# load library
library(R2jags)


# Ch 7 t-test equal variance ---------------------------------------------------

# Assumptions:
#   equal variance
#   normal dist of residuals
#   datapoints are independent
#   residuals are homoschedastic?

# generate data

# peregrine falcon wingspan by sex
n1 <- 60      # no. females
n2 <- 40      # no. males
mu1 <- 105    # female pop. mean (cm)
mu2 <- 77.5   # male pop. mean
sigma <- 2.5  # pop. sd (same for both sexes)-See assumption
n <- n1 + n2  # total sample size
y1 <- rnorm(n1, mu1, sigma)  # female data
y2 <- rnorm(n2, mu2, sigma)  # male data
y <- c(y1,y2)                # all data
x <- rep(c(0,1),c(n1,n2))    # indicator variable for sex (male = 1, female = 0)
dev.off()
boxplot(y ~ x, xlab = "Female = 0; Male = 1",
        ylab = "Wingspan (cm)")

# effects parameterization data generation-REVIEW THIS
alpha <- mu1          # females as the intercept
beta <- mu2 - mu1     # diff bt males and females
E.y <- alpha + beta*x # expected value for each sample (linear predictor)
y.obs <- rnorm(n, E.y, sigma) # data
boxplot(y.obs ~ x, xlab = "Female = 0; Male = 1",
        ylab = "Wingspan (cm)") #in jags, you can have as many factors as you want, but you need to start as ints (1,2,3)

# frequentist model analysis
fit1 <- lm(y ~ x)
fit2 <- lm(y.obs ~ x)
summary(fit1)
summary(fit2) #p value shows difference is stastically significant

t.test(y ~ x, var.equal = TRUE)
t.test(y.obs ~ x, var.equal = TRUE)

# model matricies
model.matrix(fit1)
model.matrix(fit2)


# JAGS model
sink("ttest.txt")
cat(" 
  model {

# PRIORS
mu1 ~ dnorm(0, 0.001)
delta ~ dnorm(0, 0.001)
sigma ~ dunif(0, 10)
tau <- 1/sigma ^ 2

# LIKELIHOOD Do these first adn then specify priors based on what you needed v
for(i in 1:n){
  y[i] ~ dnorm(mu[i], tau) #mean is linear predictor
  mu[i] <- mu1 + delta * x[i] #x is the dummy variable we created (rows of 0s and 1s). Using mean of females (x = 0.) When x = 1, will add the difference btwn the mean of females and the difference btwn m/f to get males mean
  
  resid[i] <- y[i] - mu[i] #residuals, resudual error

} # i

# calculate mean for males
mu2 <- mu1 +delta #outside loop so will calc mu2 once. Will then be updated in loop

} # end of model  
    ",fill = TRUE)
sink()

# data for the model
win.data <- list(y = y, #Wingspan data
                 x = x, #dummy variable for sex
                 n = n) #nymber of samples
                  
# Initial values
inits <- function() list(mu1 = rnorm(1), #If you choose really bad init vals, code will break
                         delta = rnorm(1),
                         sigma = rlnorm(1)#Cant use norm bc that give negative vals. could also use unif
                         ) #using random num generator because we don't want to influence random num generator

# Parameters monitored
params <- c("mu1",
            "mu2",
            "delta",
            "resid",
            "sigma") #What results we want to get

# MCMC settings
ni <- 1000  #no. iterations for mcmc
nt <- 1     #thinning rate
nb <- 100   #burnin, how many initial vals you want to discard bc still estimating
nc <- 3     #no. chains

# run the model and store it in an object called "out"
out <- jags(win.data,    # data
            inits,       # initial values
            params,      # parameters to monitor
            "ttest.txt", # name of model text file
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, # mcmc details 
            working.directory = getwd())

# print results
print(out, dig = 1) #going to have a long output becauase we have resids for every wingspan

# are the residuals normal?
hist(out$BUGSoutput$mean$resid, xlab = "Residual (cm)")

# see the full columns of the design matrix and linear algebra
d <- data.frame(wingspan = round(y,1), #This dataframe is full model matrix
           int_ind = rep(1,length(y)),
           int_est = round(rep(out$BUGSoutput$mean$mu1, length(y)),1),
           delt_ind = x,
           delt_est = round(rep(out$BUGSoutput$mean$delta, length(y)),1),
           resid = round(out$BUGSoutput$mean$resid,1))
#col1 observed wingspan. 
#col2 is indicitaor var for intercept
#col3 is est of what intercept is
#col4 indicator var for sex
#col5effect of being male

#ex. 79.3 = 1*(105.2) + 1*(-28.1) + 2.2 #Things in () are obs data. 2.2 is residual

# Ch 7 t-test unequal variance ---------------------------------------------------

# generate data

# peregrine falcon wingspan by sex
n1 <- 60      # no. females
n2 <- 40      # no. males
mu1 <- 105    # female pop. mean (cm)
mu2 <- 77.5   # male pop. mean
sigma1 <- 5.5  # female pop. sd
sigma2 <- 2.5
n <- n1 + n2  # total sample size
y1 <- rnorm(n1, mu1, sigma1)  # female data
y2 <- rnorm(n2, mu2, sigma2)  # male data
y <- c(y1,y2)                # all data
x <- rep(c(0,1),c(n1,n2))    # indicator variable for sex (male = 1)
dev.off()
boxplot(y ~ x, xlab = "Female = 0; Male = 1",
        ylab = "Wingspan (cm)")

# JAGS model
sink("h.ttest.txt")
cat(" 
    model {
    
    # PRIORS
    mu1 ~ dnorm(0,0.001)
    tau1 <- 1/sigma1^2
    sigma1 ~ dunif(0,10)
    mu2 ~ dnorm(0,0.001)
    tau2 <- 1/sigma2^2
    sigma2 ~ dunif(0,10) 
    
    # LIKELIHOOD #Two separate means. two loops. per females and males
    for(i in 1:n1){
    y1[i] ~ dnorm(mu1,tau1)
  }
    for(i in 1:n2){
    y2[i] ~ dnorm(mu2,tau2)
    
  } 
    
  # get the difference between the two means i.e. assign delta
  delta <- mu1-mu2
  delta_sigma <- sigma1 - sigma2
    
    } # end of model",fill = TRUE)
sink()

win.data <- list(y1 = y1, y2 = y2, n1 = n1, n2 = n2)

# Initial values
inits <- function() list(mu1 = rnorm(1), sigma1 = rlnorm(1),
                         mu2 = rnorm(1), sigma2 = rlnorm(1))

# Parameters monitored
params <- c("mu1",
            "mu2",
            "delta",
            "sigma1",
            "sigma2",
            "delta_sigma"
) 

# MCMC settings
ni <- 1000; nt <- 3; nb <- 100; nc <- 3


out <- jags(win.data, inits, params, "h.ttest.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
            working.directory = getwd())
print(out, dig = 1) #Our sigmas look pretty different. Delta sigmas we can conclude that deltas are not the same


# Ch 7 unequal var alt. parameterization --------------------------------------------
#Check this out for review

# JAGS model
sink("h.ttest.txt")
cat(" 
    model {
    
    # PRIORS
    alpha0 ~ dnorm(0,0.001)
    alpha1 ~ dnorm(0,0.001)
    beta0 ~ dunif(0,5)
    beta1 ~ dnorm(0,0.001)

    # LIKELIHOOD
    for(i in 1:n){
    y[i] ~ dnorm(mu[i],tau[i])
    
    mu[i] <- alpha0 + alpha1*x[i]

    tau[i] <- 1/sigma[i]^2

    sigma[i] <- beta0 + beta1*x[i]

    }
    } # end of model  
    ",fill = TRUE)
sink()

win.data <- list(y= y, n = n, x = x)

# Initial values
inits <- function() list(alpha0 = rnorm(1), beta0 = rlnorm(1),
                         alpha1 = rnorm(1), beta1 = rnorm(1))

# Parameters monitored
params <- c("alpha0",
            "alpha1",
            "beta0",
            "beta1"
) 

# MCMC settings
ni <- 10000; nt <- 1; nb <- 1000; nc <- 3


out <- jags(win.data, inits, params, "h.ttest.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
            working.directory = getwd())
print(out, dig = 2)


# Ch 8 linear regression ---------------------------------------------------

# generate data
# percentage of quadrats with wallcreepers observed ("occupied")
# in Switzerland for years 1990-2005
n <- 16 # number of years of observations
a <- 40 # intercept
b <- -1.5 # slope is negative. Over times seeing fewer wall creppybois
sigma2 <- 25 # variance
x <- 1:16 # values for year
set.seed(12)
eps <- rnorm(n, mean = 0, sd = sqrt(sigma2)) # normally distributed error
y <- a + b*x + eps # observed data (percentages)


# what is another equivalent way to generate the data?- the wat we just did it was the classical way to gen data from y = mx +b plus normal error
y.alt <- rnorm(n, a+b*x, sqrt(sigma2))
plot(y,y.alt)
cor(y,y.alt)

# plot
plot((x),y, xlab = "Year since 1989", ylab = "Percent occupied",
     pch = 20)
abline(a = a, 
       b = b,
       lwd = 3)

# JAGS model
sink("linreg.txt")
cat(" 
    model {
    
    # PRIORS
    a ~ dnorm(0, .001)
    b ~ dnorm(0, .001)
    sigma ~ dunif(0,10)
    tau <- 1/sigma ^ 2
    
    # LIKELIHOOD
    for(i in 1:n){
      y[i] ~ dnorm(mu[i], tau)
      mu[i] <- a + b * x[i]

    # calculate residuals
      resid[i] <- y[i] - mu[i]
      
    } # i
    } # end of model  
    ",fill = TRUE)
sink()

win.data <- list(y = y,x = x, n = n)

# Initial values
inits <- function() list(a = rnorm(1), 
                         b = rnorm(1), 
                         sigma = rlnorm(1))

# Parameters monitored
params <- c("a",
            "b",
            "resid",
            "sigma"
) 

# MCMC settings
ni <- 1000; nt <- 3; nb <- 100; nc <- 3

# fit model
out <- jags(win.data, inits, params, "linreg.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
            working.directory = getwd())

# print results
print(out, dig = 1)

# Ch 8 deluxe linear regression ---------------------------------------------------

# generate data
# percentage of quadrats with wallcreepers observed ("occupied")
# in Switzerland for years 1990-2005
n <- 16 # number of years of observations
a <- 40 # intercept
b <- -.2 # slope
sigma2 <- 25 # variance
x <- 1:16 # values for year
set.seed(12)
eps <- rnorm(n, mean = 0, sd = sqrt(sigma2)) # normally distributed error
y <- a + b*x + eps # observed data (percentages)

# plot
plot((x),y, xlab = "Year since 1989", ylab = "Percent occupied",
     pch = 20)
abline(a = a, 
       b = b,
       lwd = 3)

# JAGS model
sink("linreg.txt")
cat(" 
    model {
    
    # PRIORS
    a ~ dnorm(0,0.001)
    b ~ dnorm(0,0.001)
    tau <- 1/sigma^2
    sigma ~ dunif(0,10)
    
    # LIKELIHOOD
    for(i in 1:n){
    y[i] ~ dnorm(mu[i],tau)
    mu[i] <- a + b*x[i]
    resid[i] <- y[i] - mu[i]   # residuals
    
    } # i
    
   # EXTRA FUN STUFF BRUH
    # here, step evaluates if b is >= to 0. If model things beta is negative, will eval to 0. Probability of decline becomes 1. But calculates every at EVERY ITERATION. Will add them up so find prob pop is actually declining. Low slopes will have p < 1
    p.decline <- 1 - step(b) # probability that population is declining
    
    # Assess model fit using sum-of-squares discrepancy measure ESSENTIALLY BAYES P VAL. How much does predicted vals from model look like the real data you collected
    
    for(i in 1:n){
    
    # squared residuals b/t model and observed data
    sq[i] <- pow(resid[i],2) # use residual calculated above DISCREPENCY MEASURE-differnece btwn what model predicts and your data. This squares resid. can also use ^
    
    # Use model to generate new data MAKING NEW DATA norm dist with mean mu and precision tau
    # And get squared residuals for that data set
    y.new[i] ~ dnorm(mu[i], tau)
    resid.new[i] <- y.new[i] - mu[i]
    sq.new[i] <- pow(resid.new[i],2) #get sum of sqs for new data
    }
    
    fit <- sum(sq[]) # sum of squares for observed data. the empty brackets are just C++ jargon
    fit.new <- sum(sq.new[]) # sum of squares for model-generated data We're going to compare these. results in proprotion of times that model generated error is bigger than real data
    test <- step(fit.new - fit)
    bpvalue <- mean(test)
    
    
    } # end of model  
    ",fill = TRUE)
sink()

win.data <- list(y = y, #Wingspan data
                 x = x, #dummy variable for sex
                 n = n) #nymber of samples

# Initial values
inits <- function() list(a = rnorm(1), 
                         b = rnorm(1), 
                         sigma = rlnorm(1))

# Parameters monitored
params <- c("a",
            "b",
            "resid",
            "sigma",
            "p.decline",
            "fit",
            "fit.new",
            #"predicted",
            #"residual",
            "bpvalue"
) 

# MCMC settings
ni <- 1000; nt <- 3; nb <- 100; nc <- 3


out <- jags(win.data, inits, params, "linreg.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
            working.directory = getwd())
print(out, dig = 1) #you want bpval to be about 0.5. p decline will tell us % chance data is declining

# checking linear regression assumption 
# are residuals  homoscedastic?

# plot the residuals against the predicted values
# visually inspect to see if there is a pattern

out <- out$BUGSoutput

plot(out$mean$predicted,out$mean$resid, 
     pch = 20,      # fill in circles
     cex = 1.75,    # make circles larger
     las = 1,       # change y axis labels to read vertically
     main = "Check residuals for homoscedasticity",
     ylab = "Residual (percentage)",
     xlab = "Predicted percentage")
abline(h = 0, lwd = 2, lty = 2)


# Ch 8 posterior predictions & bayes p-values ----------------------------------


# posterior predictive distributions and
# Bayesian p-values

# intuition: do data generated by my model 
# generally look like the data I actually collected?
# i.e., is the model a reasonable approximation of reality?

# more technically: we generate new data using the model
# calculate some discrepancy measure for that new data
# then do the same for the observed data
# then compare the two discrepancy measure; they should be similar
# i.e., the error for model-generated ("perfect") data should resemble
# the error for the observed data

# first, inspect this visually
plot(out$sims.list$fit, # sum of squared residuals for observed data
     out$sims.list$fit.new, # sum of squared residuals for model-generated data
     ylab = "SOS for model-generated datasets",
     xlab = "SOS for observed data")
abline(0,1)

# get Bayesian p-value
# proportion of times model-generated discrepancy is 
# larger than the observed data discrepancy
mean(out$sims.list$fit.new > out$sims.list$fit)

# the same as:
out$mean$bpvalue



# Ch 8 model credible intervals -------------------------------------------------------

# first a plot with no error
plot(y ~ x,
     xlab = "Years since 1990",
     ylab = "Percent occupied quadrats")
pred_y <- out$mean$a + out$mean$b * x
points(1:16,pred_y, type = "l", lwd = 3)

# now get 95% credible interval
# we want to get the expected value from the linear predictor (a + b*x) 
# at every single MCMC iteration

# create empty array to hold predictions
predictions <- array(dim = c(length(out$sims.list$a), # rows = no. mcmc iteractions
                             length(x))) # columns = number of years
dim(predictions)

# get predictions
for(i in 1: length(x)){
  predictions[ ,i] <- out$sims.list$a + out$sims.list$b * x[i]
}

# get the upper 97.5%ile and lower 2.5%ile of these predictions
up <- apply(predictions,2,quantile, probs = 0.975)
low <- apply(predictions,2,quantile, probs = 0.025)

# add these to the plot
points(1:16, up, type = "l", lwd = 2, col = "gray")
points(1:16, low, type = "l", lwd = 2, col = "gray")


