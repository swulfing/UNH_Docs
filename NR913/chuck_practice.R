setwd("C:/Users/sophi/Documents/UNH_Docs/NR913")

library(R2jags)

chucks <- read.csv("mystery_chucks.csv")
hist(chucks$woodchucks)

sink("chuckpractice.txt")
cat(" 
  model {

#Hints:
#Woodchucks doen't come out when its cold
#temp shouldn't predict how many. temp is probably more whether or not they're coming out. same with rain. Doesn't really mean how many
#forage could effect both presence and number and predators may dictate fewer woodchucks
#therefore two processes to consider
#

    # PRIORS
#May need to make loop for different rains?
    alpha ~ dnorm(0,0.001)
    beta1  ~ dnorm(0,0.001)
    beta2  ~ dbern(.5)#check
    beta3 ~ dnorm(0,0.001) #something before 
    beta4 ~ dnorm(0,0.001)



# LIKELIHOOD Do these first adn then specify priors based on what you needed
    for(i in 1:n){
    
     w[i] ~ dbern(psi) #Adding a term
     chucks[i] ~ dpois(w[i]*lambda[i])
     log(lambda[i]) <- alpha + beta1*temp[i] + beta2*rain[i] + beta3*forage[i] + beta4*predators[i]
     
    } # i

 # # PRIORS
 #    beta0 ~ dnorm(0,0.001)  
 #    beta1 ~ dnorm(0,0.001)  
 #    psi ~ dunif(0,1)
 #    
 #    # LIKELIHOOD
 #    for(i in 1:n){
 # 
 #    w[i] ~ dbern(psi) #Adding a term
 #    hares[i] ~ dpois(w[i]*lambda[i])
 #    log(lambda[i]) <- beta0 + beta1 * landuse[i] 

} # end of model  
    ",fill = TRUE)
sink()

# data for the model
win.data <- list(mean.density = hares$mean.density,
                 landuse      = as.factor(hares$landuse),
                 year         = hares$year - min(hares$year) + 1, # transform
                 n            = nrow(hares),
                 nrain     = length(unique(hares$landuse)))

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
out <- jags(win.data,
            inits,
            params,
            "chuckpractice.txt",
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb,
            working.directory = getwd())

# print results
print(out, dig = 1) #going to have a long output becauase we have resids for every wingspan