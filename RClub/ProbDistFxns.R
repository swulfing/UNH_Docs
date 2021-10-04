## Probability Distribution Functions and Quantile plots ##

#All distributions have a total area under the curve of 1



#Uniform Dist-All values are equally likely. 
#Normal distribution (AKA Gaussian). A standard normal dist has a mean of 0 and sd of 1. Normal distributions defined by mean (centered) and SD (67% of values are 1 SD from mean. 95% of values are 2 sd from mean).

#Three fxns
#probability, density, quanitle. 
#Density: y value
#quantile: x value
#prob: area under curve

#plot(seq(-1,1,.01)), dnorm(seq(-4,4,.01), xlab = "quantile")

dnorm(0)
dnorm(-1.96)
dnorm(50, mean =48, sd = 2)

pnorm(0) #area to left of 0 (mean)
pnorm(-1.96)
pnorm(0, mean = 1, sd = 5)

#To calculate area to right of curve:
pnorm(-1.96, lower.tail = FALSE)
#or
1-pnorm(-1.96)

#what is the quantile of the value (which x val gives you an area of (x))
qnorm(.5)
qnorm(.025)
qnorm(.025, lower.tail = FALSE)


#t distributions. Used with finite sample sizes
dt(0, 2) #x, degrees of freedom
pt(2, 10) #quant, df
qt(.05, 19) #pval, df

qt(.025, 24, lower.tail = FALSE)

#F distribution. F statistic is a ratio of variances (V). V can never be negative because it comes from sums of squares.
#Numerator: between group variance (distance between means of different independent means)
#Denom: Within group variance (distanes between measurements within groups)

#F stat will be greater than 1 if you can conclusively say that groups are different. Fundamental in ANOVA

qf(.025, 20, 21) #Last two numbers are degrees of freedoms
qf(.025, 20, 21, lower.tail = FALSE)

#Quantile Quantile plots useful in that can be used to assess normality

#Pretend we measured these values (observations) and now we want to see if they fall in normal dist
(observations <- rnorm(10,16,4))
mean(observations)
sd(observations)
#Chop up theoretical curve, find midpoints, then plot your dist. if they line up well, then follows dist


#Skewedness or kertosis(peakedness)

observations <- observations[order(observations)]
observations

#now to f dist
observations <- rf(10,10,12)
#Chop up theoretical curve, find midpoints, then plot your dist. if they line up well, then follows dist



#Chi sq is used to compare observed vs theoretical
#Roll a dice 96 times, expect to get each one 16 times. Use Chisq to determine if dice is fair
#   [(obs-exp)/exp]^2
#Sum all of these up to get overall test stat

