
# snake dataset -----------------------------------------------------------

mass   <- c(6, 8,5,7,9,11)
pop    <- factor(c(1,1,2,2,3,3)) #Factor is categorical
region <- factor(c(1,1,1,1,2,2))
region2 <- factor(c("A", "A", "A", "B", "B", "B"))
hab    <- factor(c(1,2,3,1,2,3))
svl    <- c(40, 45,39,50,52,57)

data <-cbind(mass,pop,region,hab,svl)

# model of the mean -------------------------------------------------------

# still a statistical model
# deterministic part: the mean
# random part: the variance/sd

#Mean is still a type of model
m <- lm(mass ~ 1) # ~ as a function of
summary(m)

model.matrix(m) #Will give model matrix of linear models



# t-test ------------------------------------------------------------------

# effects parameterization
# note the reference category
m <- lm(mass ~ region) #Effects parametizationAlha (intercept) ismean of region 1 snakes. Beta is region 2-difference between means of snakes. P value is effective of difference between 1 and 2
m <- lm(mass ~ region - 1) #mean parametization. P vals are both existence of mass in both regions (snakes are real!)
m2 <- lm(mass ~ region2)#only thing this is showing you is that R will list either aplhabetically or numerically depending on what the datatype
summary(m2)

summary(m)
model.matrix(m) #Different depending on if you're doing Effects or mean parameterization
#doing a t test in base r assumes variances are equal (called welches t test) so if your variances ARE different, you will get different results between the two


# means parameterization
# simpler to interpret but no p-value for the comparison
m <- lm(mass ~ region -1)
summary(m)
model.matrix(m)

# remember: statistically, these two models are the same



# simple linear regression ------------------------------------------------


m <- lm(mass ~ svl)#intercept is mass of snek with no length
summary(m)
model.matrix(m)
model.matrix(~ svl)

svl_p <- seq(min(svl),max(svl),length.out = length(svl))

svl_preds <- m$coefficients[1] + m$coefficients[2]*svl_p

plot(mass ~ svl)
lines(svl_preds ~ svl_p)


# one-way anova -----------------------------------------------------------

# effects parameterization
m <- lm(mass ~ pop) #pop has 3 cats. intercept is mean mass of pop1. all other estimates are differences of means compared to pop 1. Effects parameterization
summary(m)
model.matrix(m)

# means parameterization
m <- lm(mass ~ pop - 1) #Only have 1 degrere of freedom so st error is gonna be weird. Here, estimates are means at each pop. NOT telling us if pops are different from eachother
summary(m)
model.matrix(m)


# two-way anova  ----------------------------------------------------

#two categorical explan variables habitat has 3 cats, region has 2 cats
# effects parameterization
m <- lm(mass ~ region + hab) #intercept: mean of snakes in region 1, habitat 1. region2 Effect of region 2 compared to region 1, averaged across habitats
#hab2 is effect of hap 3 ompared to region 1 averaged across regions
summary(m)
model.matrix(m)

# means parameterization - but not really 
# because can't only be in a habitat, 
# must also be in a region
m <- lm(mass ~ region + hab -1) #Hard to mean parameterize because you cant be in a region and not a habitat
summary(m)
model.matrix(m) #region 1 and region 2 means work fine. 

# interaction effects parameterization
# note: we run out of df because there are no reg 2 snakes in hab 1
m <- lm(mass ~ region * hab ) #mass is responding to two things at once. Because our examples is so small it runs out of dfs so it can't interpret everything.
#comparing to region1: region 2 across habs, hab 1 across regions, hab 2 across regions, and then region2 hab2 vs 1, region 2 hab3 against 1
summary(m)
model.matrix(m)
#if you ever have interactive effects, plot first!!!! Hard to understand interactions


# means parameterizatoin
m <- lm(mass ~ region * hab - 1 - region - hab)#Can look at relations btwn all individual combos
summary(m)
model.matrix(m)

# ancova ------------------------------------------------------------------

#1 discrete, 1 cont
# main effects - effects parameterization
# effect of svl not dependent upon pop
m <- lm(mass ~ pop + svl) #2 effects and 1 slope of length?
summary(m)

plot(svl,mass,col=c(rep("red",2),rep("blue",2),rep("green",2)), pch = 20)
abline(m$coef[1],m$coef[4],lwd = 4, col="red")
abline(m$coef[1]+m$coef[2],m$coef[4], lwd = 4, col="blue")
abline(m$coef[1]+m$coef[3],m$coef[4], lwd = 4, lty = 2, col="green")
#Lines are different but all have same slope

# main effects - mean parameterization
# hard to interpret with so little data
m <- lm(mass ~ pop  + svl -1) #Estimate for each thing. some will be negs
model.matrix(m)
summary(m)
data

# interactive effects
# effects of svl depend on which pop the snake is in
# i.e., there are different slopes and different intercepts
# effects parameterization
m <- lm(mass ~ pop * svl) #mass vs snl depends on what pop you're in. comparing each thing to pop1. This is really hard to interpret
summary(m)
model.matrix(m)

#plotting makes this easier. Slopes and interceprs are different
plot(svl,mass,col=c(rep("red",2),rep("blue",2),rep("green",2)))
abline(m$coef[1],m$coef[4],lwd = 4,col="red")
abline(m$coef[1]+m$coef[2],m$coef[4] + m$coef[5],lwd = 4,col="blue")
abline(m$coef[1]+m$coef[3],m$coef[4] + m$coef[6],lwd = 4,col="green")

# means parameterization
# easier to interpet b/c we get three slopes
m <- lm(mass ~ pop * svl - svl -1) #means paramterization
summary(m)
model.matrix(m)


