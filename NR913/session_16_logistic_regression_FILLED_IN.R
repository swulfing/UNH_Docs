# load library
library(R2jags)

purps <- "#440154FF"
midnight <- "#414487FF"
dkteal <- "#2A788EFF"
limabean <- "#7AD151FF"
bananas <- "#FDE725FF"

ccolors <- c(purps, midnight, dkteal,limabean, bananas)

# load data & explore ---------------------------------------------------------------

# GOAL: model pine marten distribution and evaluate habitat relationships

# load data
mart <- read.csv("marten_data_xy.csv")

# summarize presence and array data using table
table(mart$mart_pres)
table(mart$Array)


names(mart)

# visualize covariate distributions
par(mfrow = c(2,2))
hist(mart$bwoodland,
     main = "Broadleaf woodland",
     col = limabean,
     xlab = "Proportion broadleaf woodland")
hist(mart$cwoodland,
     main = "Conifer woodland",
     col = dkteal,
     xlab = "Proportion conifer woodland")
hist(mart$rain,
     main = "Rainfall",
     col = midnight,
     xlab = "Total rainfall (mm/year)")
hist(mart$dist_blg,
     main = "Building proximity",
     col = bananas,
     xlab = "Distance to nearest building (km)")
dev.off()

# quantify collinearity (cor) 
d <- data.frame(bwood = mart$bwoodland,
           cwood = mart$cwoodland,
           d_blg = mart$dist_blg,
           rain = mart$rain)

round(cor(d),2)


# visualize correlations
d <- data.frame(mart = mart$mart_pres01,
                bwood = mart$bwoodland,
                cwood = mart$cwoodland,
                d_blg = mart$dist_blg,
                rain = mart$rain)

pairs(d, pch = 19)

# create scaled versions of covariates
mart$BWOOD <- scale(mart$bwoodland)[,1]
mart$CWOOD <- scale(mart$cwoodland)[,1]
mart$D_BLG <- scale(mart$dist_blg)[,1]
mart$RAIN  <- scale(mart$rain)[,1]

# visualize
plot(mart$rain,mart$RAIN,
     ylab = "Rain in SD",
     xlab = "Rain in mm/yr",
     pch = 19)
hist(mart$bwoodland, breaks = 25,
     main = "Broadleaf woodland", xlab = "Proportion")
hist(mart$BWOOD, breaks = 25,
     main = "Broadleaf woodland", xlab = "Scaled (sd)")

# logistic regression model with one covariate -------------------------------------------------

# JAGS model 
sink("logistic.txt")
cat(" 
    model {
    
    # PRIORS
    beta0 ~ dnorm(0,0.001)
    beta1 ~ dnorm(0,0.001)

    # LIKELIHOOD
    for(i in 1:nsite){
    mart[i] ~ dbern(p[i])
    
    logit(p[i]) <-  beta0 + beta1 * cwood[i]

    }  # i
    } # end of model  
    ",fill = TRUE)
sink()

win.data <- list(nsite = nrow(mart),
                 cwood = mart$CWOOD,
                 mart = mart$mart_pres01)

# Initial values
inits <- function()list(beta0 = rnorm(1),
                        beta1 = rnorm(1))

# Parameters monitored
params <- c("beta0","beta1") 

# MCMC settings
nc <- 3; nt <- 1; ni <- 3000; nb <- 500

# run the model
out <- jags(win.data, inits, params, "logistic.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
            working.directory = getwd())
print(out, dig = 2)

# check convergence visually
traceplot(out)

# visualizations ----------------------------------------------------------

# plot parameter posteriors
o <- out$BUGSoutput$sims.list

# parameter posteriors (logit scale) #Doesn't cross zero. Therefore posterior means slope is real and strong efffect. Pine woodlants effect martens positively
d <- density(o$beta0)
d2 <- density(o$beta1)

plot(d,
     xlab = "Posterior distribution",
     main = "Intercept")
polygon(d, col = midnight, 
        border = "black", lwd = 3)
plot(d2,
     xlab = "Posterior distribution",
     main = "Slope")
polygon(d2, col = dkteal, 
        border = "black", lwd = 3)

# effects plot of conifer on pine marten occurrence

# prep some data for predictions
oo <- out$BUGSoutput
int <-   as.numeric(oo$mean$beta0)
slope <- as.numeric(oo$mean$beta1)
cwood_values <- seq(min(mart$CWOOD),
                    max(mart$CWOOD),
                    length.out = nrow(mart))

# model predictions
preds <- plogis(int + slope * cwood_values)

# plot- right now not showing any uncertainty
plot(preds ~ cwood_values,
     type = "l",
     lwd = 3,
     las = 1,
     frame.plot = FALSE,
     xlab = "Proportion conifer (scaled)",
     ylab = "Marten occurrence prob.",
     ylim = c(0,1))

# add in presence/absence data points
points(mart$mart_pres01 ~ mart$CWOOD,
       pch = "|")

# plot on the natural scale of the covariate
cwood_values_nat <- seq(min(mart$cwoodland),
                        max(mart$cwoodland),
                        length.out = nrow(mart))

plot(preds ~ cwood_values_nat,
     type = "l",
     lwd = 3,
     las = 1,
     frame.plot = FALSE,
     xlab = "Proportion conifer",
     ylab = "Marten occurrence prob.",
     ylim = c(0,1))
points(mart$mart_pres01 ~ mart$cwoodland,
       pch = "|") #x axis still scaled if you use CWOOD instead of cwoodland. Wont chane plot, just x aid



# creating credible interval visualizations -------------------------------

#Taking out top and bottom 2.5% to find 95% interval where we're sure that the mean was captured in
#Graph all intercepts and all slopes and make a prediction for every one
ints <- as.numeric(o$beta0)
slopes <- as.numeric(o$beta1)

# matrix to hold predictions. columns will be cwood from -1.07 to 1.73 (range of CWOOD or standardized values). Will get an occupancy prediction for every cwood
pred_matrix <- matrix(NA,
                      nrow = length(ints),
                      ncol = nrow(mart)) #no cwood variable i.e. every datapoint

# loop to get predictions
for(i in 1:length(cwood_values)){
  pred_matrix[ ,i] <- plogis(ints + slopes * cwood_values[i]) #plogis bc Inverse of log link that we used
}#every prediction for each mcmc iteration
  

# get upper and lower quantiles of predictions at every covariate value
lower <- apply(pred_matrix,2,quantile, prob = 0.025) #apply applies a function to an array. 2 is saying to apply to cols instead of rows, quantile is the function we will apply, prob is what quantile we want
upper <- apply(pred_matrix,2,quantile, prob = 0.975)

# add to plot
plot(preds ~ cwood_values_nat,
     type = "l",
     lwd = 4,
     las = 1,
     frame.plot = FALSE,
     xlab = "Proportion conifer",
     ylab = "Marten occurrence prob.",
     ylim = c(0,1))
lines(lower ~ cwood_values_nat, lwd = 2, lty = 6)
lines(upper ~ cwood_values_nat, lwd = 2, lty = 6)
points(mart$mart_pres01 ~ mart$cwoodland,
       pch = 20)

# another option
# plot a random subset of 200 predictions from the pred matrix
ss <- sample(1:length(ints),200) #take a random subset of predictions

plot(preds ~ cwood_values_nat,
     type = "l",
     lwd = 4,
     las = 1,
     frame.plot = FALSE,
     xlab = "Proportion conifer",
     ylab = "Marten occurrence prob.",
     ylim = c(0,1))

# add 200 random mcmc predictive lines. Add in lines for each random sample
for(i in 1:200){
lines(pred_matrix[ss[i],] ~ cwood_values_nat,
      col = "gray")
}

# add back the mean line
lines(preds ~ cwood_values_nat,
     type = "l",
     lwd = 4)

# add in data points
points(mart$mart_pres01 ~ mart$cwoodland,
       pch = 20)


# logistic regression model with four covariates -------------------------------------------------

# JAGS model 
sink("logistic.txt")
cat(" 
    model {
    
    # PRIORS
    beta0 ~ dnorm(0,0.001)
    beta1 ~ dnorm(0,0.001)
    beta2 ~ dnorm(0,0.001)
    beta3 ~ dnorm(0,0.001)
    beta4 ~ dnorm(0,0.001)


    # LIKELIHOOD
    for(i in 1:nsite){
    mart[i] ~ dbern(p[i])
    
    logit(p[i]) <-  beta0 + beta1 * bwood[i]
    + beta2 * cwood[i]
    + beta3 * d_blg[i]
    + beta4 * rain[i]


    }  # i
    } # end of model  
    ",fill = TRUE)
sink()

win.data <- list(nsite = nrow(mart),
                 bwood = mart$BWOOD,
                 cwood = mart$CWOOD,
                 d_blg = mart$D_BLG,
                 rain = mart$RAIN,
                 mart = mart$mart_pres01)

# Initial values
inits <- function()list(beta0 = rnorm(1),
                        beta1 = rnorm(1))

# Parameters monitored
params <- c("beta0","beta1","beta2","beta3","beta4") 

# MCMC settings
nc <- 3; nt <- 1; ni <- 3000; nb <- 500

# run the model
out_4 <- jags(win.data, inits, params, "logistic.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
            working.directory = getwd())
print(out_4, dig = 2)

# check convergence visually
traceplot(out_4)


# logistic regression model with four covariates -------------------------------------------------

# JAGS model BAYES P VAL. calc residuals (errors). then calc residuals for data model generates. Then square them, sum them, then compare. They should be similar (0.5). If they're dissimilar, will get close to 0 or 1
sink("logistic.txt")
cat(" 
    model {
    
    # PRIORS
    beta0 ~ dnorm(0,0.001)
    beta1 ~ dnorm(0,0.001)
    beta2 ~ dnorm(0,0.001)
    beta3 ~ dnorm(0,0.001)
    beta4 ~ dnorm(0,0.001)


    # LIKELIHOOD
    for(i in 1:nsite){
    mart[i] ~ dbern(p[i])
    
    logit(p[i]) <-  beta0 + beta1 * bwood[i]
    + beta2 * cwood[i]
    + beta3 * d_blg[i]
    + beta4 * rain[i]
    
    # RESIDUALS. With poisson, mean and var are connected so this will throw off residuals at high vals. Pearson residual corrects for that (see presid exn below)
    presid[i] <- (mart[i]-p[i])/sqrt(p[i]*(1-p[i]))
    mart.new[i] ~ dbern(p[i]) #This generates data from our model
    presid.new[i] <- (mart.new[i]-p[i])/sqrt(p[i]*(1-p[i]))
    
    p2[i] <- presid[i]^2
    p.new2[i] <- presid.new[i]^2 #square so that negs don't cancel out positive resids

    }  # i
    
    D <- sum(p2[])
    D.new <- sum(p.new2[])
    
    bpvalue <- step(D.new/D-1) #step function tests if whatever is in () is below or above/= 0.

    } # end of model  
    ",fill = TRUE)
sink()

win.data <- list(nsite = nrow(mart),
                 bwood = mart$BWOOD,
                 cwood = mart$CWOOD,
                 d_blg = mart$D_BLG,
                 rain = mart$RAIN,
                 mart = mart$mart_pres01)

# Initial values
inits <- function()list(beta0 = rnorm(1),
                        beta1 = rnorm(1))

# Parameters monitored
params <- c("beta0","beta1","beta2","beta3","beta4","bpvalue",
            "presid", "D", "D.new") 

# MCMC settings
nc <- 3; nt <- 1; ni <- 3000; nb <- 500

# run the model
out_4_bp <- jags(win.data, inits, params, "logistic.txt", 
              n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
              working.directory = getwd())
print(out_4_bp, dig = 2)

# spline correlograms for spatial autocorrelation
#if you have spatial correlation in response variable and spatial correlation in covariates, they will potentially take care of spatial autocorrelation. If theyre not good, spatial autocorrelation remains.
#It't not that your model assumes your sites are independent, the assumption is model residuals are independent 
#After you've accounted for spatial covaraites, residuals in model can't ahve spatial autocorrelation

#We've created pearson residuals already that we can plot to see correlation 
#spline correlograms for spatial autocorrelation
install.packages("ncf")
library(ncf)
?spline.correlog

spp <- spline.correlog(x = mart$X,
                       y = mart$Y,
                       z = as.numeric(out_4_bp$BUGSoutput$mean$presid))
plot(spp) #Takes a long time cause lookin at every residual combo. we have 466 locations and is running every combo
#Shows correlaiton of pearson as a fxn of distance. Want confidece envelope to cross zero. W/in 10000 meters, residuals are correlated. Within array we hae problem. Also 150000 has some kind of correlation. we're not sure why. Correlation isn't huge (~.2). Slight problem. pseudo replication. model is more confident than it should be.

# logistic regression model with four covariates + RI -------------------------------------------------

# JAGS model 
#first way to fix, give each array of 20 cameras it's own intercept. Acknowledging spatial autocorrelation.
#beta 0 for evey camera array
sink("logistic.txt")
cat(" 
    model {
    
    # PRIORS
    for(i in 1:narray){
    beta0[i] ~ dnorm(mu,tau) #need a prior for each intercept
    }
    mu ~ dnorm(0,0.001)
    tau <- 1/sigma^2
    sigma ~ dunif(0,5)
    
    beta1 ~ dnorm(0,0.001)
    beta2 ~ dnorm(0,0.001)
    beta3 ~ dnorm(0,0.001)
    beta4 ~ dnorm(0,0.001)


    # LIKELIHOOD
    for(i in 1:nsite){
    mart[i] ~ dbern(p[i])
    
    logit(p[i]) <-  beta0[array[i]] + beta1 * bwood[i]
    + beta2 * cwood[i]
    + beta3 * d_blg[i]
    + beta4 * rain[i]
    
    # RESIDUALS
    presid[i] <- (mart[i]-p[i])/sqrt(p[i]*(1-p[i]))
    mart.new[i] ~ dbern(p[i])
    presid.new[i] <- (mart.new[i]-p[i])/sqrt(p[i]*(1-p[i]))
    
    p2[i] <- presid[i]^2
    p.new2[i] <- presid.new[i]^2

    }  # i
    
    D <- sum(p2[])
    D.new <- sum(p.new2[])
    
    bpvalue <- step(D.new/D-1)

    } # end of model  
    ",fill = TRUE)
sink()

win.data <- list(nsite = nrow(mart),
                 bwood = mart$BWOOD,
                 cwood = mart$CWOOD,
                 d_blg = mart$D_BLG,
                 rain = mart$RAIN,
                 mart = mart$mart_pres01,
                 array = as.integer(as.factor(mart$Array)), #Currently in character format. Need to make factor hten int
                 narray = length(unique(mart$Array)))

# Initial values
inits <- function()list(
                        beta1 = rnorm(1)) #now have 24 inital values for b0. Can either supply 24 or none and hope it works. Some param estimates get changed. Good to try with nested spatial data

# Parameters monitored
params <- c("beta0","beta1","beta2","beta3","beta4","bpvalue",
            "presid") 

# MCMC settings
nc <- 3; nt <- 1; ni <- 3000; nb <- 500

# run the model
out_4_bp_ri <- jags(win.data, inits, params, "logistic.txt", 
                 n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                 working.directory = getwd())
print(out_4_bp_ri, dig = 2)

# spline correlograms for spatial autocorrelation
# install.packages("ncf")
# library(ncf)
# ?spline.correlog

spp <- spline.correlog(x = mart$X,
                       y = mart$Y,
                       z = as.numeric(out_4_bp_ri$BUGSoutput$mean$presid))
plot(spp)
