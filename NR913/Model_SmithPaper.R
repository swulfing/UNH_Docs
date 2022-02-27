# JAGS model
sink("smithPaper.txt")
cat(" 
  model {

# PRIORS

alpha_hat ~ dnorm(0, 1000)
b_rkm ~ dnorm(0, 1000)
b_netDepth ~ dnorm(0, 1000)
b_distDown ~ dnorm(0, 1000)
b_distUp ~ dnorm(0, 1000)
b_distDetect ~ dnorm(0, 1000)
sigma ~ unif(0,10)
tau <- 1/sigma ^ 2


# LIKELIHOOD

  for (j in 1:s){ #OK so it's a little weird how I did this. I have 3 for loops, one going throught each site (j terms) another going through each net series index (i terms) and then each data point withing that (k terms) I realize if we were using an actual dataset, this might iterate funny unless I were to set up data correctly, but otherwise would this be the general way to set up this model?
  
    alpha[j] ~ dnorm(alpha_hat, tau)
    
    for(i in 1:n){
      eta[i] <- log(time_net_series_deployed)
      
      for(k in 1:d){
    
        c_ij[k] ~ dpois(exp(log_link)) #Fix log link fxn
        log_link <- eta[i] + alpha[j] + b_rkm*x[k] + b_netDepth*x[k] + b_distDown*x[k] + b_distUp*x[k] + b_distDetect*x[k] #Log Link Function

    }
  }

} # end of model  
    ",fill = TRUE)
sink()

# data for the model
win.data <- list(c_ij = c_ij, #Catch per net series
                 x = x, #FIX THIS
                 d = d, #length of data within each net series index and site
                 n = n, #Net series index
                 s = s) #Site

# Initial values
inits <- function() list(alpha_hat = rnorm(1), #Kept the norms of previous classes even though I don't know enough about the data to say if this is a really bad init choice or not
                         b_rkm = rnorm(1),
                         b_netDepth = rlnorm(1), #double check
                         b_distDown = rnorm(1),
                         b_distUp = rnorm(1),
                         b_distDetect = rnorm(1),
                         sigma = runif(1, 0, 10) #I just copied the uniform dist assumed for sigma above. Is that picking too close of an initial value?
)


# Parameters monitored
params <- c("alpha_hat",
            "b_rkm",
            "b_netDepth",
            "b_distDown",
            "b_distUp",
            "b_distDetect",
            "sigma")



# MCMC settings
ni <- 20000
nt <- 1
nb <- 5000
nc <- 3

# run the model and store it in an object called "out"
out <- jags(win.data,
            inits,
            params,
            "smithPaper.txt",
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb,
            working.directory = getwd())

gelman.diag(out, confidence = 0.95, transform=FALSE, autoburnin=TRUE,
            multivariate=TRUE) #Rhat val #This is called a Brooks-Gelman Rubin Stat. It will essentially spit out a "point estimate of the potential scale reduction factor" and upper confidence limit (Here, the paper accepted values less than 1.1 as indicating convergence of chain) GOOGLE WHAT THIS IS DOING/WHAT IT MEANS, jags does this anyways

# print results
print(out, dig = 1)

# ???? here, step evaluates if b is >= to 0. If model things beta is negative, will eval to 0. Probability of decline becomes 1. But calculates every at EVERY ITERATION. Will add them up so find prob pop is actually declining. Low slopes will have p < 1
p.decline <- 1 - step(b) # probability that population is declining
