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

# mart <- read.csv("Session11_ANOVA/marten_data.csv")
# Scotland, 466 camera traps around scotland.
# presence/abs data
# habitat related covaraites



# summarize presence and array data using table

table(mart$mart_pres)#ways to visualize data. Look at presence abs
table(mart$Array)

names(mart)

# visualize covariate distributions
#covariates don't have to be normally distributed
#broadleaf woodland is very rare. We can't predict marten dist in places that are 100% brodaleaf woodland
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

# quantify collinearity (cor) - corrlation in covariates (predictor variables)
#Correlation is potentially a problem because highly correlated covariates. Our model is trying to explain variance.
#one will explain data and the other (the weaker) will explain the leftover variation
#When trying to interpret/infer mechanisms. collinearity can complicate that interpretation
#If your only goal is to predict response variable, collinearity is not a problem. Can actually be good for making predictions
d <- data.frame(bwood = mart$bwoodland,
           cwood = mart$cwoodland,
           d_blg = mart$dist_blg,
           rain = mart$rain)

round(cor(d),2) #pearsons correlation. Gives mtx of each var with each other var. Want to be below 0.7 ... 0.6 is better
#None of the covariates are correlated

# visualize correlations
d <- data.frame(mart = mart$mart_pres01,
                bwood = mart$bwoodland,
                cwood = mart$cwoodland,
                d_blg = mart$dist_blg,
                rain = mart$rain)

pairs(d, pch = ) #will show you hoe data is correlated. Go through each combo

# create scaled versions of covariates. All variables are going into same model
# add columns to dataframe and capitalize them
mart$BWOOD <- scale(mart$bwoodland)[,1] #[,1] just takes the scaled params. There are other things in the scale fxn list
mart$CWOOD <- scale(mart$cwoodland)[,1]
mart$D_BLG <- scale(mart$dist_blg)[,1]
mart$RAIN  <- scale(mart$rain)[,1]

# visualize- Not changing pattern of variable, we're transforming it. x is old dataset y is transformed dataset
#Also known as z score transforming
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
      beta0 ~ dnorm(0, .001)
      beta1 ~ dnorm(0, .001)
      #p is defined by betas, p is defined by the betas
      #These priors can actually mess up logistic regressions, but we'll get back to that
      
      
      
    # LIKELIHOOD
      
      for(i in 1:nsite){
      
        mart[i] ~ dbern(p[i])
        
        #Now write linear predictor
        logit(p[i]) <- beta0 + beta1 * cwood[i]
        
      }
    } # end of model  
    ",fill = TRUE)

sink()

win.data <- list(nsite = nrow(mart),
                 cwood = mart$CWOOD,#use standardized dataset
                 mart = mart$mart_pres01 #using the 0/1 column
                   )

# Initial values - Anything that isn't date needs an intial val. Betas
inits <- function()list(beta0 = rnorm(1),
                        beta1 = rnorm(1)
                          )

# Parameters monitored
params <- c("beta0", "beta1") 

# MCMC settings
nc <- 3; nt <- 1; ni <- 3000; nb <- 500#thinning rate. only keep every  xvalue. More complex models need more iterations. if it doesn't converge just add more. First try, run with 10 iterations and see if code has a but.

# run the model
out <- jags(win.data, inits, params, "logistic.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
            working.directory = getwd())
print(out, dig = 2)

# check convergence visually
traceplot(out) #Shows you all the catepillar chains. bad if one of the colors jumps or if they all do that's really bad. #rhat tells you how much they all match with eachother. If this happens choose different inits

#rhat is 1
#95% doesn't cross zero (in this case it does slightly)
#my.vect is param. sd vect. is sd of param
#REMEMBER THIS IS STILL THE LOGIT SCALE


# visualizations ----------------------------------------------------------

# plot parameter posteriors
o <- out$BUGSoutput$sims.list #simslist is raw output of mcmc

# parameter posteriors (logit scale)-density plot of posteriors
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
#package called bayesplot that does good visulaizations but all your data needs to be in nice tidyverse format

# effects plot of conifer on pine marten occurrence

# prep some data for predictions
oo <- out$BUGSoutput
int <- as.numeric(oo$mean$beta0) #bugs output is a list so need as.numeric just the mean of the posterior dist graphed above
slope <- as.numeric(oo$mean$beta1)
cwood_values <- seq(min(mart$CWOOD),
                    max(mart$CWOOD),
                    length.out = nrow(mart))#want length.out to be same number as length of dataset
#dont want to graph data. cwood was sampled really sparsely. create vector from min val of cwood to max val of cwood

# model predictions-need to back transform form logit fxn
preds <- plogis(int + slope * cwood_values)

# plot
plot(preds ~ cwood_values,
     type = "l",
     lwd = 3,
     las = 1,
     frame.plot = FALSE,
     xlab = "Proportion conifer (scaled)",
     ylab = "Marten occurrence prob.",
     ylim = c(0,1))

# add in presence/absence data points
points()

# plot on the natural scale of the covariate
cwood_values_nat <- 

plot(preds ~ cwood_values_nat,
     type = "l",
     lwd = 3,
     las = 1,
     frame.plot = FALSE,
     xlab = "Proportion conifer",
     ylab = "Marten occurrence prob.",
     ylim = c(0,1))
points(mart$mart_pres01 ~ mart$cwoodland,
       pch = "|")



# creating credible interval visualizations -------------------------------

ints <- 
slopes <- 

# matrix to hold predictions
pred_matrix <- 

# loop to get predictions

  

# get upper and lower quantiles of predictions at every covariate value
lower <- apply()
upper <- apply()

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
ss <- sample(1:length(ints),200)

plot(preds ~ cwood_values_nat,
     type = "l",
     lwd = 4,
     las = 1,
     frame.plot = FALSE,
     xlab = "Proportion conifer",
     ylab = "Marten occurrence prob.",
     ylim = c(0,1))

# add 200 random mcmc predictive lines
for(i in 1:200){

  
}

# add back the mean line
lines(preds ~ cwood_values_nat,
     type = "l",
     lwd = 4)

# add in data points
points(mart$mart_pres01 ~ mart$cwoodland,
       pch = 20)

