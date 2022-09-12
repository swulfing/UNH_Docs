# load library
library(R2jags)

purps <- "#440154FF"
midnight <- "#414487FF"
dkteal <- "#2A788EFF"
limabean <- "#7AD151FF"
bananas <- "#FDE725FF"

# load green crab dataset
crabs <- read.csv("Session11_ANOVA/green_crab.csv")

# boxplot
boxplot(cpue ~ site, 
        ylab = "Catch per unit effort (crabs/wk)",
        xlab = "Site",
        outline = TRUE,
        notch = FALSE,
        staplewex = 0.2,
        boxwex = 0.4,
        border = TRUE,
        col = c(purps,limabean,bananas),
        data = crabs)

#more than two cats, so its an anova, not a ttest. Cats are in integer form

class(crabs$site)

# green crab lm anova -------------------------------------------------------------------

an <- lm(cpue ~ as.factor(site), data = crabs)#need to convert to as factor
summary(an)

anova(an) #Asks if there's a difference between of the cats


# green crab anova in JAGS -----------------------------------------------------------

# JAGS model
sink("anova.txt")
cat(" 
    model {
    
    # PRIORS
    for(i in 1:nsites){       #loop for alphas. can also do alpha[1] alpha[2]
    alpha[i] ~ dnorm(0,0.001)
    }
    tau <- 1/sigma^2
    sigma ~ dunif(0,100) #This gives only 1 variance across all sites
    
    # LIKELIHOOD
    for(i in 1:n){
    cpue[i] ~ dnorm(mu[i],tau)
    mu[i] <- alpha[site[i]] #Mean will vary by site. 3 alphas, 1 per site. loop through every row of your dataset.
    } # i
    
    # derived quantitites
    diff23 <- alpha[3] - alpha[2]

    # generate new data from the fitted model
    for(i in 1:n){
    y.new[i] ~ dnorm(mu[i],tau)   #Generates new data for each run of mcmc
    }
    
    } # end of model  
    ",fill = TRUE)
sink()

win.data <- list(cpue   = crabs$cpue,
                 site   = crabs$site, 
                 n      = nrow(crabs),
                 nsites = length(unique(crabs$site)))

# Initial values
inits <- function()list(alpha = rnorm(3), sigma = rlnorm(1))

# Parameters monitored
params <- c("alpha",
            "sigma",
            "diff23",
            "y.new"
) 

# MCMC settings
ni <- 1000; nt <- 3; nb <- 100; nc <- 3


out <- jags(win.data, inits, params, "anova.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
            working.directory = getwd())
print(out, dig = 1)

# plot alpha 1 - the mean of Site 1. ITS NOT every possible val that cpue can take. its mean of all those
hist(out$BUGSoutput$sims.list$alpha[,1], #Plot posterieor of alpha using a histogram
     xlab = "Alpha1",
     main = NULL,
     breaks=seq(0,30,1),
     col = midnight)

# smooth density plot of alpha 1
d <- density(out$BUGSoutput$sims.list$alpha[,1]) #smooth density plot

plot(d,
     xlab = "Alpha1",
     main = "")
polygon(d, col = bananas, border = purps, lwd = 3)

# boxplot of actual data vs that from fitted model
# show 8 plots from 8 mcmc runs
par(mfrow = c(3,3),
    mar = c(5.1, 4.1, 3.1, 0.5)) #b, l ,t ,r
boxplot(cpue ~ site, 
        ylab = "CPUE",
        xlab = "Site",
        staplewex = 0.2,
        boxwex = 0.4,
        frame.plot = FALSE,
        col = c(purps,limabean,bananas),
        data = crabs,
        main = "Actual data")
for(i in 1:8){ #Maps data with equal variance, model is predicting negative cpues. This is where we need a link fxn. bc cpue of site 3 is low, it will dip below 0
boxplot(out$BUGSoutput$sims.list$y.new[i,] ~ crabs$site, 
        ylab = "CPUE",
        xlab = "Site",
        staplewex = 0.2,
        boxwex = 0.4,
        frame.plot = FALSE,
        col = c(purps,limabean,bananas),
        main = paste0("Model-generated data; iter = ",i))
}
dev.off()

# pine marten anova - fixed effect ----------------------------------------

# load data
mart <- read.csv("Session11_ANOVA/marten_data.csv")

# let's focus on arrays with > 200 mean elevation
mean_ele <- tapply(mart$elevation, 
                   as.factor(mart$Array), 
                   FUN = mean)
mean_ele
over100 <- names(mean_ele)[which(mean_ele > 200)]

#FIXTHIS
#mart <- mart[1:240,]#tHIS DIDNT WORK EITHER
mart <- mart[mart$Array %in% over100,] #THis is nt indexing correctly, its maintaining original numbers of level of dataset instead of renumbering them

# means by each array
mean_ele <- tapply(mart$elevation, 
                   as.factor(mart$Array), 
                   FUN = mean)
mean_ele
# stan dev by each array
sd_ele <- tapply(mart$elevation, #base r version of dplyr apply
                   as.factor(mart$Array), 
                   FUN = sd)
sd_ele

# elevation as a function of array (study site)
boxplot(elevation ~ Array, 
        ylab = "Elevation (masl)",
        xlab = " ",
        las = 2,
        outline = TRUE,
        notch = FALSE,
        staplewex = 0.2,
        boxwex = 0.4,
        frame.plot = FALSE,
        border = TRUE,
        xaxt="n",
        col = c(purps,midnight,dkteal,limabean,bananas),
        data = mart)
axis(1,
     cex.axis=0.6,
     at = 1:length(unique(mart$Array)),
     labels = sort(unique(mart$Array)),
     las = 2)

# JAGS model same thing but with 12 sites, not 3
sink("anova.mart.txt")
cat(" 
    model {
    
    # PRIORS
    for(i in 1:nsites){
    alpha[i] ~ dnorm(0,0.001)
    }
    tau <- 1/sigma^2
    sigma ~ dunif(0,200)
    
    # LIKELIHOOD
    for(i in 1:n){
    ele[i] ~ dnorm(mu[i],tau)
    mu[i] <- alpha[array[i]]
    } # i
    
    # derived quantitites
    diff23 <- alpha[3] - alpha[2]
    } # end of model  
    ",fill = TRUE)
sink()

# data
win.data <- list(ele    = mart$elevation,
                 array  = as.integer(as.factor(mart$Array)), #Takes string (mart$array) then converts to factor then integer
                 n      = nrow(mart),
                 nsites = length(unique(mart$Array)))

# Initial values
inits <- function()list(alpha = rnorm(length(unique(mart$Array))), 
                        sigma = rlnorm(1))

# Parameters monitored
params <- c("alpha",
            "sigma",
            "diff23"
) 

# MCMC settings
ni <- 1000; nt <- 3; nb <- 100; nc <- 3

# model out
out <- jags(win.data, inits, params, "anova.mart.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
            working.directory = getwd()) #Sigma is 200. the prior sets it to 200. changing this will alter the output sigma
print(out, dig = 1)

# plot means
avg <- out$BUGSoutput$mean$alpha
stdeviation <- out$BUGSoutput$mean$sigma


# plot means of the model
plot(1:12, out$BUGSoutput$mean$alpha,
     ylim=range(c(avg-rep(stdeviation,12), mean_ele+sd_ele)),
     pch=19, cex = 1.5, xlab="Array", ylab="Mean +/- SD",
     main="Scatter plot with std.dev error bars",
     
)
# hack: we draw arrows but with very special "arrowheads"
arrows(1:12, avg-rep(stdeviation,12),1:12, avg+rep(stdeviation,12), 
       length=0.05, 
       angle=90,
       lwd = 3, 
       code=3)

# plot the actual data
points(1.2:12.2, mean_ele,
     ylim=range(c(mean_ele-sd_ele, mean_ele+sd_ele)),
     pch=19, cex = 1.5,col = dkteal
)
arrows(1.2:12.2, mean_ele-sd_ele,1.2:12.2, mean_ele+sd_ele, 
       length=0.05, 
       angle=90, 
       lwd = 3, 
       code=3, 
       col = dkteal)

# What happened?!
#Didn't standardize data


# pine marten anova - fixed effect stand ----------------------------------------

# JAGS model
sink("anova.s.txt")
cat(" 
    model {
    
    # PRIORS
    for(i in 1:nsites){
    alpha[i] ~ dnorm(0,0.001)
    }
    tau <- 1/sigma^2
    sigma ~ dunif(0,10)
    
    # LIKELIHOOD
    for(i in 1:n){
    ele[i] ~ dnorm(mu[i],tau)
    mu[i] <- alpha[array[i]]
    } # i
    
    # derived quantitites
    diff23 <- alpha[3] - alpha[2]
    stan_dev <- (sigma*stdev_data) 

    for(i in 1:nsites){
    avg[i] <- (alpha[i]*stdev_data) + mean_data
    }

    } # end of model  
    ",fill = TRUE)
sink()

# data
win.data <- list(ele    = scale(mart$elevation)[,1], #only difference, scaling response variable
                 array  = as.integer(as.factor(mart$Array)), 
                 n      = nrow(mart),
                 nsites = length(unique(mart$Array)),
                 stdev_data = sd(mart$elevation),
                 mean_data = mean(mart$elevation))

# Initial values
inits <- function()list(alpha = rnorm(length(unique(mart$Array))), 
                        sigma = rlnorm(1))

# Parameters monitored
params <- c("alpha",
            "sigma",
            "diff23",
            "avg",
            "stan_dev"
) 

# MCMC settings
ni <- 1000; nt <- 3; nb <- 100; nc <- 3

# model out
out <- jags(win.data, inits, params, "anova.s.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
            working.directory = getwd())
print(out, dig = 1) #Scaled. stats don't change with transformed data. Different interpretation. Avg is the same as alpha just back transformed

# plot means

# plots
avg <- out$BUGSoutput$mean$avg
stdeviation <- out$BUGSoutput$mean$stan_dev

# model predictions-looks a lot better if your r code actually runs lol
plot(1:12, out$BUGSoutput$mean$avg,
     ylim=range(c(avg-rep(stdeviation,12), mean_ele+sd_ele)),
     pch=19, cex = 1.5, xlab="Array", ylab="Mean +/- SD",
     main="Scatter plot with std.dev error bars",
     
)
arrows(1:12, avg-rep(stdeviation,12),1:12, avg+rep(stdeviation,12), 
       length=0.05, 
       angle=90,
       lwd = 4, 
       code=3)

# data
points(1.2:12.2, mean_ele,
       ylim=range(c(mean_ele-sd_ele, mean_ele+sd_ele)),
       pch=19, cex = 1.5,col = dkteal
)
arrows(1.2:12.2, mean_ele-sd_ele,1.2:12.2, mean_ele+sd_ele, 
       length=0.05, 
       angle=90, 
       lwd = 4, 
       code=3, 
       col = dkteal)

# pine marten anova - fixed effect stand unequal variance ----------------------------------------

# JAGS model
sink("anova.s.uneq.txt")
cat(" 
    model {
    
    # PRIORS
    for(i in 1:nsites){
    alpha[i] ~ dnorm(0,0.001)
    tau[i] <- 1/sigma[i]^2
    sigma[i] ~ dunif(0,10)
    }

    
    # LIKELIHOOD
    for(i in 1:n){
    ele[i] ~ dnorm(mu[i],tau[array[i]]) #Unique variance. gives 12 different tau vals. each tau needs proir. put sigma prior in loop
    mu[i] <- alpha[array[i]]
    } # i
    
    # derived quantitites
    diff23 <- alpha[3] - alpha[2]

    for(i in 1:nsites){
    avg[i] <- (alpha[i]*stdev_data) + mean_data
    stan_dev[i] <- (sigma[i]*stdev_data) 

    }
    
    } # end of model  
    ",fill = TRUE)
sink()

# data
win.data <- list(ele    = scale(mart$elevation)[,1],
                 array  = as.integer(as.factor(mart$Array)), 
                 n      = nrow(mart),
                 nsites = length(unique(mart$Array)),
                 stdev_data = sd(mart$elevation),
                 mean_data = mean(mart$elevation))

# Initial values
inits <- function()list(alpha = rnorm(length(unique(mart$Array))), 
                        sigma = rlnorm(length(unique(mart$Array))))

# Parameters monitored
params <- c("alpha",
            "sigma",
            "diff23",
            "avg",
            "stan_dev"
) 

# MCMC settings
ni <- 1000; nt <- 3; nb <- 100; nc <- 3

# model out
out <- jags(win.data, inits, params, "anova.s.uneq.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
            working.directory = getwd())
print(out, dig = 1)

# plot 
avg <- out$BUGSoutput$mean$avg
stdeviation <- out$BUGSoutput$mean$stan_dev

# model-always slightly overestimates varience than data. probs a good thing
plot(1:12, out$BUGSoutput$mean$avg,
     ylim=range(c(avg-stdeviation), c(avg+stdeviation)),
     pch=19, cex = 1.5, xlab="Array", ylab="Mean +/- SD",
     main="Elevation - fixed effect",
     
)
arrows(1:12, avg-stdeviation,1:12, avg+stdeviation, 
       length=0.05, 
       angle=90,
       lwd = 3, 
       code=3)

# data
points(1.2:12.2, mean_ele,
       ylim=range(c(mean_ele-sd_ele, mean_ele+sd_ele)),
       pch=19, cex = 1.5, col = dkteal
)
arrows(1.2:12.2, mean_ele-sd_ele,1.2:12.2, mean_ele+sd_ele, 
       length=0.05, 
       angle=90, 
       lwd = 3, 
       code=3, 
       col = dkteal)

# pine marten anova - random effect stand unequal variance ----------------------------------------

# JAGS model
sink("anova.random.uneq.txt")
cat(" 
    model {
    
    # PRIORS
    for(i in 1:nsites){
    alpha[i] ~ dnorm(mu_array,tau_array)
    }
    mu_array ~ dnorm(0,0.001)
    tau_array <- 1/sigma_array^2
    sigma_array ~ dunif(0,100)
    
    for(i in 1:nsites){
    tau[i] <- 1/sigma[i]^2
    sigma[i] ~ dunif(0,100)
    }
    
    # LIKELIHOOD
    for(i in 1:n){
    ele[i] ~ dnorm(mu[i],tau[array[i]])
    mu[i] <- alpha[array[i]]
    } # i
    
    # derived quantitites
    diff23 <- alpha[3] - alpha[2]

    for(i in 1:nsites){
    avg[i] <- (alpha[i]*stdev_data) + mean_data
    stan_dev[i] <- (sigma[i]*stdev_data) 
  }
    } # end of model  
    ",fill = TRUE)
sink()

# data
win.data <- list(ele    = scale(mart$elevation)[,1],
                 array  = as.integer(as.factor(mart$Array)), 
                 n      = nrow(mart),
                 nsites = length(unique(mart$Array)),
                 stdev_data = sd(mart$elevation),
                 mean_data = mean(mart$elevation))

# Initial values
inits <- function()list(mu_array = rnorm(1), 
                        sigma_array = rlnorm(1),
                        sigma = rlnorm(length(unique(mart$Array))))

# Parameters monitored
params <- c("alpha",
            "sigma",
            "diff23",
            "avg",
            "stan_dev"
) 

# MCMC settings
ni <- 1000; nt <- 3; nb <- 100; nc <- 3

# model out
out3 <- jags(win.data, inits, params, "anova.random.uneq.txt", 
             n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
             working.directory = getwd())
print(out3, dig = 1)

# plot means
out <- out3
avg <- out$BUGSoutput$mean$avg
stdeviation <- out$BUGSoutput$mean$stan_dev

# model
plot(1:12, out$BUGSoutput$mean$avg,
     ylim=range(c(avg-stdeviation), c(avg+stdeviation)),
     pch=19, cex = 1.5,  xlab="Array", ylab="Mean +/- SD",
     main="Elevation - random effect",
     
)
arrows(1:12, avg-stdeviation,1:12, avg+stdeviation, 
       length=0.05, 
       angle=90,
       lwd = 3, 
       code=3)

# data
points(1.2:12.2, mean_ele,
       ylim=range(c(mean_ele-sd_ele, mean_ele+sd_ele)),
       pch=19, cex = 1.5, col = dkteal
)
arrows(1.2:12.2, mean_ele-sd_ele,1.2:12.2, mean_ele+sd_ele, 
       length=0.05, 
       angle=90, 
       lwd = 3, 
       code=3, 
       col = dkteal)

# chicks anova ----------------------------------------

ck <- chickwts

# JAGS model
sink("chicks.txt")
cat(" 
    model {
    
    # PRIORS
    for(i in 1:nsites){
    alpha[i] ~ dnorm(0,0.001)
    }
    tau <- 1/sigma^2
    sigma ~ dunif(0,20)
    
    # LIKELIHOOD
    for(i in 1:n){
    weight[i] ~ dnorm(mu[i],tau)
    mu[i] <- alpha[feed[i]]
    } # i
    
    # derived quantitites
    stan_dev <- (sigma*stdev_data) 
    diff23 <- alpha[3] - alpha[2]

    for(i in 1:nsites){
    avg[i] <- (alpha[i]*stdev_data) + mean_data
    }


    } # end of model  
    ",fill = TRUE)
sink()

# data
win.data <- list(weight  = scale(ck$weight)[,1],
                 feed    = as.integer(ck$feed), 
                 n       = nrow(ck),
                 nsites  = length(unique(ck$feed)), #Already a factor so don't need to convert
                 stdev_data = sd(ck$weight),
                 mean_data  = mean(ck$weight))

# Initial values
inits <- function()list(alpha = rnorm(length(unique(ck$feed))), 
                        sigma = rlnorm(1))

# Parameters monitored
params <- c("alpha",
            "sigma",
            "diff23",
            "stan_dev",
            "avg"
) 

# MCMC settings
ni <- 1000; nt <- 3; nb <- 100; nc <- 3

# model out. Data is standardized Diff 23 does not cross 0 so we can conclude model is probs doing what it's supposed
out <- jags(win.data, inits, params, "chicks.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
            working.directory = getwd())
print(out, dig = 1)

# plot means
avg <- out$BUGSoutput$mean$avg
stdeviation <- out$BUGSoutput$mean$stan_dev

stdev_data <-  sd(ck$weight)
mean_data  <-  tapply(ck$weight,ck$feed,FUN = mean)

# model
plot(1:6, out$BUGSoutput$mean$avg,
     ylim=range(c(avg-rep(stdev_data,6), avg+rep(stdev_data,6))),
     pch=19, cex = 1.5, xlab="Feed", ylab="Mean +/- SD",
     main="Chick weights - fixed effect",
     
)
arrows(1:6, avg-rep(stdeviation,6),1:6, avg+rep(stdeviation,6), 
       length=0.05, 
       angle=90,
       lwd = 3, 
       code=3)

# data
points(1.1:6.1, mean_data,
       ylim=range(c(mean_data-stdev_data, mean_data+stdev_data)),
       pch=19, cex = 1.5,col = dkteal
)
arrows(1.1:6.1, mean_data-stdev_data,1.1:6.1, mean_data+stdev_data, 
       length=0.05, 
       angle=90, 
       lwd = 3, 
       code=3, 
       col = dkteal)

# chicks anova random effect ----------------------------------------

#So far these have all been fixed effects models. Assuming feeds are independent from one another. separate treatments. Random effects. What if mean vals per category come from other distribution.

#eg. years have correlations between them. Variation associated with year, would need to grab that from another model i.e. random effect

#Another way to think about it is if there are lots of other unmeasured feeds that we didnt' research

#Or if you have a suspect mean (outlier) but not very many samples at that point, having a dist for mean can regulate against that

# JAGS model
sink("chicks.re.txt")
cat(" 
    model {
    
    # PRIORS
    for(i in 1:nsites){
    alpha[i] ~ dnorm(mu_feed,tau_feed)
    }
    
    #This is hyperparamters. This is what makes it heirarchical
    mu_feed ~ dnorm(0,0.001)
    tau_feed <- 1/sigma_tau^2
    sigma_tau ~ dunif(0,20)

    tau <- 1/sigma^2
    sigma ~ dunif(0,20)
    
    # LIKELIHOOD
    for(i in 1:n){
    weight[i] ~ dnorm(mu[i],tau)
    mu[i] <- alpha[feed[i]]
    } # i
    
    # derived quantitites
    stan_dev <- (sigma*stdev_data) 
    diff23 <- alpha[3] - alpha[2]
    
    for(i in 1:nsites){
    avg[i] <- (alpha[i]*stdev_data) + mean_data
    }
    } # end of model  
    ",fill = TRUE)
sink()

# data
win.data <- list(weight  = scale(ck$weight)[,1],
                 feed    = as.integer(ck$feed), 
                 n       = nrow(ck),
                 nsites  = length(unique(ck$feed)),
                 stdev_data = sd(ck$weight),
                 mean_data  = mean(ck$weight))

# Initial values
inits <- function()list(mu_feed = rnorm(1),
                        sigma_tau = rlnorm(1),
                        sigma = rlnorm(1))

# Parameters monitored
params <- c("alpha",
            "sigma",
            "diff23",
            "stan_dev",
            "avg"
) 

# MCMC settings
ni <- 1000; nt <- 3; nb <- 100; nc <- 3

# model out
out <- jags(win.data, inits, params, "chicks.re.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
            working.directory = getwd())
print(out, dig = 2)

# plot 
avg <- out$BUGSoutput$mean$avg
stdeviation <- out$BUGSoutput$mean$stan_dev

stdev_data <-  sd(ck$weight)
mean_data  <-  tapply(ck$weight,ck$feed,FUN = mean)

# model
plot(1:6, out$BUGSoutput$mean$avg,
     ylim=range(c(avg-rep(stdev_data,6), avg+rep(stdev_data,6))),
     pch=19, cex = 1.5, xlab="Feed", ylab="Mean +/- SD",
     main="Chick weights - fixed effect",
     
)
arrows(1:6, avg-rep(stdeviation,6),1:6, avg+rep(stdeviation,6), 
       length=0.05, 
       angle=90,
       lwd = 2, 
       code=3)

# data
points(1.1:6.1, mean_data,
       ylim=range(c(mean_data-stdev_data, mean_data+stdev_data)),
       pch=19, cex = 1.5,col = dkteal
)
arrows(1.1:6.1, mean_data-stdev_data,1.1:6.1, mean_data+stdev_data, 
       length=0.05, 
       angle=90, 
       lwd = 2, 
       code=3, 
       col = dkteal)

