# load library
library(R2jags)

# colors
purps <- "#440154FF"
midnight <- "#414487FF"
dkteal <- "#2A788EFF"
limabean <- "#7AD151FF"
bananas <- "#FDE725FF"

#Fixed vs random effects
# pine marten data -----------------------------------------

# load data
mart <- read.csv("Session11_ANOVA/marten_data.csv")

# let's focus on the first 12 arrays
mart <- mart[1:240,]

# suppose elevation was very high and variable in the first array
mart$elevation[1:20] <- mart$elevation[1:20] + rnorm(20,1000,400) #Taking first 20 rows and making it very high elevation. One array much higher and more variable than other sites

# get means and sd for each array
mean_ele <- tapply(mart$elevation,mart$Array,FUN = mean)
sd_ele <- tapply(mart$elevation,mart$Array,FUN = sd)


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
    
    # DERIVED QUANTITIES
    for(i in 1:nsites){
    avg[i] <- (alpha[i]*stdev_data) + mean_data
    stan_dev[i] <- (sigma[i]*stdev_data) 
    }

    mu_array_m <- (mu_array*stdev_data) + mean_data
    sigma_array_m <- (sigma_array*stdev_data) 

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
            "avg",
            "stan_dev",
            "mu_array_m",
            "sigma_array_m"
) 

# MCMC settings
ni <- 1000; nt <- 3; nb <- 100; nc <- 3

# model output
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
     ylim=range(c(avg-stdeviation), c(mean_ele+sd_ele)),
     pch=19, cex = 1.5,  xlab="Array", ylab="Mean +/- SD",
     main="Elevation - random effect",
     
)#black is model-predicted data

#Random effects: param estimates come from its own distribution
#red line: mean of means
#Gray lines: SD of hyperdist

# grand mean
abline(h = out$BUGSoutput$mean$mu_array_m,
       col = "darkred",
       lty = 2,
       lwd = 3)

# grand mean +/- one SD
abline(h = out$BUGSoutput$mean$mu_array_m + out$BUGSoutput$mean$sigma_array_m,
       col = "gray",
       lty = 2,
       lwd = 3)
abline(h = out$BUGSoutput$mean$mu_array_m - out$BUGSoutput$mean$sigma_array_m,
       col = "gray",
       lty = 2,
       lwd = 3)
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

  #Differences, error is slightly higher in the model (makes sense, model incorporates all error possible)
  #Model pulls extreme values into mean. Want to do it in case extreme may have been measured badly or if there were not enough samples. Do you think that data generation process actually has hierarchical structure (i.e. elevations normally distributed or something)
  #Reasons not to do random effect: less than 5 categories (Bolker, 2008). Models can have a hard time converging and hard to get a distribution from less than 5 pts


# green crabs data -------------------------------------------------------------
# load data
crabs <- read.csv("Session11_ANOVA/green_crab.csv")

#See slideshow for which models have continuous responses

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

# boxplot
boxplot(cpue ~ removal, 
        ylab = "Catch per unit effort (crabs/wk)",
        xlab = "Removal",
        outline = TRUE,
        notch = FALSE,
        staplewex = 0.2,
        boxwex = 0.4,
        border = TRUE,
        col = c(purps,limabean,bananas),
        data = crabs)

# green crab main 2-way -------------------------------------------------------------------

# effects parameterization- by default
an <- lm(cpue ~ as.factor(site) + as.factor(removal), data = crabs)
summary(an)

anova(an)

# reminder: no way to fit a means parameterization
# of this model using lm()

# green crab two way main anova in JAGS -----------------------------------------------------------

# JAGS model - effects parameterization. it's easier to do means in jags but we're doing effects
#Assuming no interaction i.e. removal effects each site in the same way

sink("anova.two.txt")
cat(" 
    model {
    
    # PRIORS
    
    # reference category (site 1, no removal)
    beta0 ~ dnorm(0,0.001) #Ref category intercept of site 1 and no removal for effects param
    
    # effect of site - manually set first site to zero
    beta.site[1] <- 0 #manually set reference params to zero to be a effects param
    beta.site[2] ~ dnorm(0,0.001)
    beta.site[3] ~ dnorm(0,0.001)
    
    # effect of removal - manually set first site to zero
    beta.removal[1] <- 0 #manually set reference params to zero to be a effects param
    beta.removal[2] ~ dnorm(0,0.001)
    
    # prior for variance
    tau <- 1/sigma^2
    sigma ~ dunif(0,100)
    
    # LIKELIHOOD
    for(i in 1:n){
    cpue[i] ~ dnorm(mu[i],tau)
    mu[i] <- beta0 + beta.site[site[i]] + beta.removal[removal[i]]
    } # i
    
    # generate new data from the fitted model
    for(i in 1:n){
    y.new[i] ~ dnorm(mu[i],tau)
    }
    
    diff23 <- beta.site[2] - beta.site[3] #this is just another way to interpret
    
    } # end of model  
    ",fill = TRUE)
sink()

win.data <- list(cpue   = crabs$cpue,
                 site   = crabs$site,
                 removal = as.factor(crabs$removal),
                 n      = nrow(crabs))

# Initial values
inits <- function()list(beta0 = rnorm(1), 
                        sigma = rlnorm(1))

# Parameters monitored
params <- c("beta0",
            "beta.site",
            "beta.removal",
            "sigma",
            "diff23",
            "y.new"
) 

# MCMC settings
ni <- 1000; nt <- 3; nb <- 100; nc <- 3


out <- jags(win.data, inits, params, "anova.two.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
            working.directory = getwd())
print(out, dig = 1) #Should be similar to regular anova
#no effect of removal because 2.5 CI and 97.5% CI cross zero (significantly)

# plot beta.removal[2] - the effect of removal
hist(out$BUGSoutput$sims.list$beta.removal[,2],
     main = "Effect of crab removal",
     xlab = "Posterior distribution",
     col = midnight)

# smooth density plot 
d <- density(out$BUGSoutput$sims.list$beta.removal[,2])

plot(d,
     xlab = "Posterior distribution",
     main = "Effect of crab removal")
polygon(d, col = bananas, border = purps, lwd = 3)

# boxplot of all the parameters
o <- out$BUGSoutput$sims.list

# place all MCMC output into a dataframe
d <- data.frame(
  parameter = rep(c("beta0","beta.site2","beta.site3","beta.removal"),
                  each = length(o$beta0[,1])),
  value = c(o$beta0[,1],o$beta.site[,2],o$beta.site[,3],o$beta.removal[,2])
)

# reorder the levels for the plot
d$parameter <- factor(d$parameter, 
                      levels=c("beta0", "beta.site2", "beta.site3", 
                               "beta.removal"))

# plot-gives estimated parameters
boxplot(value ~ parameter, 
        ylab = "Value",
        xlab = "Parameter",
        outline = TRUE,
        notch = FALSE,
        staplewex = 0.2,
        boxwex = 0.4,
        border = TRUE,
        col = c(purps,midnight,limabean,bananas),
        data = d)

# green crab interaction 2-way -------------------------------------------------------------------

# effects parameterization
an <- lm(cpue ~ as.factor(site) * as.factor(removal), data = crabs)
summary(an)

# means parameterization 
cpue <- crabs$cpue
site <- as.factor(crabs$site)
removal <- crabs$removal

an <- lm(cpue ~ site * removal - 1 - removal - site)
summary(an) #gives new params-removal in site 2 and 3. To get mean you need Site 1 + Site 2 + site 2 with removal

# green crab two way interaction anova in JAGS -----------------------------------------------------------

# JAGS model - means parameterization
sink("anova.two.int.txt")
cat(" 
    model {
    
    # PRIORS
    
    # priors for all combinations
    for(i in 1:nsites){
    for(j in 1:nremoval){
    group.mean[i,j] ~ dnorm(0,0.001)
    }
    }#avoids triple bracketing. means for each site/yn thing
    
    # prior for variance
    tau <- 1/sigma^2
    sigma ~ dunif(0,100)
    
    # LIKELIHOOD
    for(i in 1:n){
    cpue[i] ~ dnorm(mu[i],tau)
    mu[i] <- group.mean[site[i],removal[i]]
    } # i
    
    # generate new data from the fitted model
    for(i in 1:n){
    y.new[i] ~ dnorm(mu[i],tau)
    }
    
    } # end of model  
    ",fill = TRUE)
sink()

win.data <- list(cpue     = crabs$cpue,
                 site     = crabs$site,
                 removal  = as.factor(crabs$removal),
                 n        = nrow(crabs),
                 nsites   = length(unique(crabs$site)),
                 nremoval = length(unique(crabs$removal)))

# Initial values
inits <- function()list(group.mean = matrix(rnorm(6),
                                            nrow = 3, ncol = 2), 
                        sigma = rlnorm(1))

# Parameters monitored
params <- c("group.mean",
            "sigma",
            "y.new"
) 

# MCMC settings
ni <- 1000; nt <- 3; nb <- 100; nc <- 3


out <- jags(win.data, inits, params, "anova.two.int.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
            working.directory = getwd())
print(out, dig = 1) #Compare to lm

# boxplot of means
o <- out$BUGSoutput$sims.list$group.mean
dim(o)

# create dataframe for plotting
d <- data.frame(
  parameter = rep(c("Site1_N","Site2_N","Site3_N",
                    "Site1_Y","Site2_Y","Site3_Y"), each = dim(o)[1]),
  value = c(o) # converts the 3-dimension array to a vector
)


# plot
boxplot(value ~ parameter, 
        ylab = "Value",
        xlab = "Parameter",
        outline = TRUE,
        notch = FALSE,
        staplewex = 0.2,
        boxwex = 0.4,
        border = TRUE,
        col = c(limabean,bananas),
        data = d) #output ow has 3 dimensions


# Swiss hare data ---------------------------------------------------------

# load hares dataset
hares <- read.table("hares.txt", header = TRUE)

# boxplot
boxplot(mean.density ~ landuse, 
        ylab = "Hare density",
        xlab = "Landuse",
        outline = TRUE,
        notch = FALSE,
        staplewex = 0.2,
        boxwex = 0.4,
        border = TRUE,
        col = c(purps,limabean,bananas),
        data = hares)

boxplot(mean.density ~ year, 
        ylab = "Hare density",
        xlab = "Year",
        outline = TRUE,
        notch = FALSE,
        staplewex = 0.2,
        boxwex = 0.4,
        border = TRUE,
        col = c(purps,limabean,bananas),
        data = hares)

# hare lm ancova ---------------------------------------------------------------

# main effects
anc <- lm(mean.density ~ landuse + year, data = hares)
summary(anc)#landusegrass is the t test (intercept is weird to interpret-mean hare density at year 0 ) year doesn't seem to have an effect, there's not really a slope

# interactive effects 

# transform year to match the JAGS code below for comparison
hares$year2 <- hares$year-1991
anc <- lm(mean.density ~ landuse * year2 , data = hares)
summary(anc) #interadtion between grassland and year IS significant

# plot densities by year by landuse
par(mfrow = c(1,2))
boxplot(mean.density ~ year, 
        ylab = "Hare density",
        xlab = "Year",
        main = "Grassland",
        outline = TRUE,
        notch = FALSE,
        staplewex = 0.2,
        boxwex = 0.4,
        border = TRUE,
        col = limabean,
        data = hares[hares$landuse == "grass",])
boxplot(mean.density ~ year, 
        ylab = "Hare density",
        xlab = "Year",
        main = "Arable",
        outline = TRUE,
        notch = FALSE,
        staplewex = 0.2,
        boxwex = 0.4,
        border = TRUE,
        col = bananas,
        data = hares[hares$landuse == "arable",]) #Hares are declining in grassland but not in arable
dev.off()

# hares interactive ancova in JAGS -----------------------------------------------------------

# JAGS model - means parameterization
sink("ancova.hares.txt")
cat(" 
    model {
    
    # PRIORS
    for(i in 1:nlanduse){ # n landuse is two. 2 alphas 2 betas
    alpha[i] ~ dnorm(0,0.001)  # intercepts
    beta[i]  ~ dnorm(0,0.001)  # slopes
    }
    
    # prior for variance
    tau <- 1/sigma^2
    sigma ~ dunif(0,100)
    
    # LIKELIHOOD
    for(i in 1:n){
    mean.density[i] ~ dnorm(mu[i],tau)
    mu[i] <- alpha[landuse[i]] + beta[landuse[i]] * year[i] #two alphas (intercepts) and slopes
    } # i

     } # end of model  
    ",fill = TRUE)
sink()

win.data <- list(mean.density = hares$mean.density,
                 landuse      = as.factor(hares$landuse),
                 year         = hares$year - min(hares$year) + 1, # transform
                 n            = nrow(hares),
                 nlanduse     = length(unique(hares$landuse)))

# Initial values
inits <- function()list(alpha = rnorm(2),
                        beta  = rnorm(2),
                        sigma = rlnorm(1))

# Parameters monitored
params <- c("alpha",
            "beta",
            "sigma"
) 

# MCMC settings
ni <- 5000; nt <- 3; nb <- 1000; nc <- 3

out <- jags(win.data, inits, params, "ancova.hares.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
            working.directory = getwd())
print(out, dig = 2) #base model thinks hares are increasing in arable, jags thinks decreasing. beta 1 overlaps 0 therefore bayesian model is technically not significant
