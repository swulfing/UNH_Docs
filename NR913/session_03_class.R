
# pine simulation ---------------------------------------------------------

# let's create a fake dataset
# white pines in college woods
# suppose there are 567 pines
# true mean is 21.6m
# true sd is 4.6
# assume the heights are normally distributed

set.seed(42)

pines <- rnorm(n = 567, mean = 21.6, sd = 4.6)
pines

t_mean <- 21.6
t_sd <- 4.6
t_median <- median(pines)
t_range <- max(pines) - min(pines)

# sample pines
pines_s <- sample(x = pines,
                  size = 25,
                  replace = FALSE)
pines_s

# sample
s_mean <- mean(pines_s)
s_sd <- sd(pines_s)
s_median <- median(pines_s)
s_range <- max(pines_s) - min(pines_s)

# put all the above into a table
t <- data.frame(dataset = c("true","sample"),
                mean = c(t_mean, s_mean),
                sd = c(t_sd, s_sd),
                median = c(t_median,s_median),
                range = c(t_range,s_range)
)

choose(567,25)


# create a loop

# create empty object to store results
pines_sample_means <- NA

for(i in 1:40000){
  pines_s <- sample(x = pines, # x is the vector to sample
                    size = 25, # size is the # samples
                    replace = FALSE) # sample w/o replacement
  pines_sample_means[i] <- mean(pines_s)
}
pines_sample_means

# is pines_sample_means normally distributed?
hist(pines_sample_means, col ="wheat")
hist(pines)

# visualizing distributions -----------------------------------------------

dev.off() #gets rid of plots

# normal
par(mfrow = c(3,2)) #puts six plots onto one
m <- 5
s <- 2
y <- rnorm(1000, m, s)

hist(y, main = paste("Mean = ", m, ": SD = ", s, sep = ""))#sep is what you're putting between

m <- 10
s <- 2
y <- rnorm(1000, m, s)

hist(y, main = paste("Mean = ", m, ": SD = ", s, sep = ""))

m <- 100
s <- 2
y <- rnorm(1000, m, s)

hist(y, main = paste("Mean = ", m, ": SD = ", s, sep = ""))
m <- 200
s <- 2
y <- rnorm(1000, m, s)

hist(y, main = paste("Mean = ", m, ": SD = ", s, sep = ""))

m <- 1000
s <- 2
y <- rnorm(1000, m, s)

hist(y, main = paste("Mean = ", m, ": SD = ", s, sep = ""))

m <- 10000
s <- 2
y <- rnorm(1000, m, s)

hist(y, main = paste("Mean = ", m, ": SD = ", s, sep = ""))

#Keeiping the SD the same will have the same spread, regardless of mean. So SD = 2, 95% of data will be within 4 of mean


# can loop to achieve the same:
# clear the plot parameters
dev.off()

# set up 6 panels
par(mfrow = c(3,2))

m <- c(5,10,100,200,1000,10000)
s <- 2

for(i in 1:length(m)){
  y <- rnorm(1000, mean = m[i], s)
  hist(y, main = paste("Mean = ", m[i], ": SD = ", s, sep = ""))
}


# keep mean the same, vary the sd
dev.off()
par(mfrow = c(3,2))

m <- 100
s <- c(2,4,8,16,32,1000)

  for(i in 1:length(s)){
    y <- rnorm(1000, mean = m, s[i])
    hist(y, main = paste("Mean = ", m, ": SD = ", s[i], sep = ""))
  }

# poisson distribution
y <-rpois(10000, lambda = 3)
hist(y, main = "Poisson, mean = 3")
  
  
dev.off()
par(mfrow = c(3,2))

lambda <- c(1,2,5,10,100,200)
for (i in 1:length(lambda)){
  y <- rpois(10000, lambda = lambda[i])
  hist(y,
       main = paste("mean = ",lambda[i],"; sd = ",lambda[i],sep = ""))  
}


# gamma distribution
y <- rgamma(1000, shape = 2, rate = 0.1)
hist(y, main = "Gamma, shape = 2, rate = 0.1")


dev.off()
par(mfrow = c(3,2))

shape <- c(0.1,1,2,5,10,100)
rate <- 0.1

for (i in 1:length(shape)){
  y <- rgamma(1000, shape = shape[i], rate = rate)
  hist(y,
       main = paste("shape = ",shape[i],"; rate = ",rate,
                    "; mean = ",shape[i]/rate,
                    "; sd = ",round(sqrt(shape[i])/rate,1),sep = ""))  
}



# HOMEWORK #1: explore what happens when shape is held constant but rate varies



# sampling distributions: gamma and poisson -----------------------------------------------

set.seed(141)

# create the dataset using a random number generator
# gamma distribution that is right-skewed
pines_g <-rgamma(n = 567,
                 shape = 0.9,
                 rate = 0.08)           # sample size is 567 white pine trees

par(mfrow = c(1,1))
hist(pines_g, xlab = "White pine height (m)", 
     main = "Entire population - Gamma", 
     col = "darkred")

# create a loop
pines_sample_means <- NA

for(i in 1:100000){
  pines_s <- sample(x = pines_g, # x is the vector to sample
                    size = 25, # size is the # samples
                    replace = FALSE) # sample w/o replacement
  pines_sample_means[i] <- mean(pines_s)
}
pines_sample_means
#Distributions of sample means. Mean of means would be close to true means. Standard error is standard dev of the graph. So SE = 2.6. Think of it as 67% chance real pop mean will be 10+-2.6. 67% chance it's within 1 SD of sample mean
sd(pines_sample_means) 



# histogram
hist(pines_sample_means, xlab = "White pine height (m)", 
     main = "Sample means - Gamma", 
     col = "red") #Will still make normal dist

# both plots
par(mfrow = c(2,1))
hist(pines_g, xlab = "White pine height (m)", 
     main = "Entire population - Gamma", 
     col = "darkred")

hist(pines_sample_means, xlab = "White pine height (m)", 
     main = "Sample means - Gamma", 
     col = "red")




############# poisson distribution describes counts ###

# suppose the population is the number of offspring that bobcats have


set.seed(141)
# create the dataset using a random number generator
bobs_p <- rpois(n = 567,       # sample size is 567 bobats
                lambda = 3) #avg number of babies = 3

par(mfrow = c(1,1))
hist(bobs_p, xlab = "Number of bobcat offspring", 
     main = "Entire population - Poisson", 
     col = "darkblue")

# create a loop
bobs_sample_means <- NA

for(i in 1:20000){
  bobs_s <- sample(x = bobs_p,        # x is the vector to sample
                    size =  25,       # size is the number of samples to take
                    replace = FALSE)  # this means sample without replacement
  
  bobs_sample_means[i] <- mean(bobs_s)
}

# hist
hist(bobs_sample_means, xlab = "Number of bobcat offspring", 
     main = "Sample means - Poisson", 
     col = "red")

# HOMEWORK 1: repeat the above but with another distribution (e.g., uniform) Beta, neg binomial, whatever you want


# how SAMPLE SIZE affects the sampling distribution -----------------------

# let's return to a normal distribution
set.seed(141)

# create the dataset using a random number generator
# the random number generator here assumes a normal distribution
pines <- rnorm(n = 567,       # sample size is 567 white pine trees
               mean = 21.6,   # mean tree height is 21.6 m
               sd = 8.6)


# create a loop
pines_sample_means <- NA
samp_sizes <- c(23, 12, 100, 39, 50, 2)
par(mfrow = c(3,2))

for (j in 1:length(samp_sizes)){
for(i in 1:20000){
  pines_s <- sample(x = pines,                   # x is the vector to sample
                    size =  samp_sizes[j],       # size is the number of samples to take
                    replace = FALSE)             # sample without replacement
  
  pines_sample_means[i] <- mean(pines_s)
}
  
  hist(pines_sample_means, xlab = "White pine height (m)", 
       main = paste("Sample means, sample size = ",samp_sizes[j],sep = ""), 
       col = "yellow2")
       abline(v = mean(pines), col = "darkslateblue", lwd = 4)#This puts the true mean in blue  
}
#Smaller sample size has larger standard dev over the mean of sample means. Therefore larger standard error


# HOMEWORK 1: repeat the above but with another distribution (e.g., poisson)


# simple linear model simulation -----------------------------------------------------

# lets model some imaginary penguins
# yi = b0 + b1x1 where pengiuns are yi and b1 is krill

beta0 <- 1.5     # intercept. No krill, on avg there are 1.5 pengiuns just boppin
beta1 <- 3.5    # slope. Strong positive relationship
x <- runif(100, 0, 10)         # some covariate, let's say its krill density

det_data <- beta0 + beta1 * x           # det for deterministic - no error here, not a statistical model yet. R is doing vector math-multiply beta1 by every input of x

dev.off()
par(mfrow = c(1,1))
plot(det_data ~ x, ylab = "Penguin Density(indiv/km2)", xlab = "Krill density (indiv/m2)")

set.seed(71)

# add error
data <-  det_data + rnorm(100, 0, 3) #This adds random noise to our sample. Error wiill have normal(mean = 0, sd = 3) or norm(0,3)
 
plot(data ~ x, ylab = "Penguin Density(indiv/km2)", xlab = "Krill density (indiv/m2)")

# fit a linear model. With all this error can we recover b0 and b1
mod <- lm(data ~ x) #linear regression. data will be y. Penguins as a function of kirll
summary(mod) #intercept b0, x is b1
mod$coefficients # look at the coefficients (the estimates for beta0 and beta1)

#The Std. Error in summary(lm) calclates a standard error for parameters B0 and B1. t val and p val are also ways to give info. p = std. error +- 2 t value. If difference overlaps 0, p is not signficant.

plot(data ~ x, ylab = "No. penguins", xlab = "Krill density (indiv/m2)")
abline(a = mod$coefficients[1], lwd = 3,  b = mod$coefficients[2])
abline(a = beta0, b = beta1, lwd = 2,lty = 2, col = "blue")

# HOMEWORK #1: simulate another dataset, plot it, and run a linear model on it
