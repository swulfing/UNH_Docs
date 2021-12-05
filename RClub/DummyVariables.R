
# The Linear Model and the Analysis of Variance


# Dummy Variables

lm(iris$Sepal.Length ~ iris$Species)

# We only have one predictor variable in this model.
# Why are there two coefficients in this model?

# It's because the variable 'Species' is categorical. THINK LINEAR MIXED MODEL CONVO

# There are three unique 'Species'.

unique(iris$Species)

# In order to perform linear regression on a
# categorical variable, dummy variables must be used.
# Dummy variables are a numeric way of representing
# categorical variables.

# Here is an example of what dummy variables are.

My_Data <- iris
My_Data$versicolor <- ifelse(iris$Species == 'versicolor', 1, 0) #Makes 3 columns where 1 means it's that species and 0 means its not
My_Data$setosa <- ifelse(iris$Species == 'setosa', 1, 0)
My_Data$virginica <- ifelse(iris$Species == 'virginica', 1, 0)
View(My_Data)

# We just created 3 new columns (one for each
# species).

# We could actually get by with just two columns.

My_Data <- My_Data[, which(colnames(My_Data) != 'virginica')]
View(My_Data)

# In this case, we got rid of the 'virginica'
# column, but we can still tell if a species is
# 'virginica' or not - if it's not 'versicolor' or
# 'setosa', it must be 'virginica'.

# In short, for a given categorical variable with n
# unique categories, we use n - 1 dummy variables to
# create our model.

lm(iris$Sepal.Length ~ iris$Species) #n-1 dummy variables

# That's why we have two, and not one or three, slopes
# in this model.

# In this case, 'setosa' is the baseline, and
# 'versicolor' has an average sepal length 0.930 units
# greater than that of 'setosa'. 'virginica' has an
# average sepal length 1.592 units greater than that
# of 'setosa'.

# Here's what the model actually looks like:

# Sepal.Length = 5.006 + (0.930 * Speciesversicolor) + (1.582 * Speciesvirginica)

# Remember that the 'Speciesversicolor' and
# 'Speciesvirginica' variables are either '0's or
# '1's. If both are '0', then the species must be
# 'setosa', and the sepal length is predicted to be
# 5.006, which is the mean sepal length for the
# species 'setosa'.

mean(iris$Sepal.Length[iris$Species == 'setosa'])


# Analysis of Variance


# How does the analysis of variance work?

Quantile <- seq(0, 27, 0.01)
Group_1 <- dnorm(Quantile, 10, 2)
Group_2 <- dnorm(Quantile, 17, 2)
Group_3 <- dnorm(Quantile, 19, 2)
plot(Group_1 ~ Quantile, type = 'l', col = 1, main = 'Looking for Differences\nBetween 3 Groups\nWhich All Have the Same Variance', xlab = 'Quantile', ylab = 'Density')
lines(Group_2 ~ Quantile, col = 2)
lines(Group_3 ~ Quantile, col = 3)
legend('topright', legend = paste("Group", 1:3), lty = 1, col = 1:3)

# Group 1 has a mean of 10, group 2 has a mean of 17,
# and group 3 has a mean of 19. How can we tell if
# they are significantly different?

# It depends on their variances. Since each group has
# the same variance, we can easily calculate a
# critical distance that can serve to indicate if
# groups are different. You can see how much group 2
# and group 3 overlap, so it's a safe bet to say that
# these groups aren't different. Group 1, on the other
# hand, is quite a bit lower than the other two, and
# its probability distribution function doesn't
# overlap much with the other two. It's safe to say
# that group 1 is probably different than the other
# two groups.

# In practice, groups never have the same exact
# variance. The analysis of variance works by
# calculating a weighted average of the variances
# of the groups. Then, based on this variance, a
# critical distance is calculated. This critical
# distance is the distance between means that two
# groups have to be apart to be deemed different.
# The critical distance is based on the weighted-
# average variance.

# There are two key assumptions this analysis makes.

# First, observations within groups must be
# normally-distributed. Another way of saying this is
# that the residuals must be normally-distributed.
# Residuals are based on a fitted model, and if the
# model predicts that an observation will be the mean
# of the group it's in, then the residuals will be the
# distance between each observation and the mean of
# the group it's in. If these residuals are normally-
# distributed, then the observations within each group
# will also be normally-distributed - you're
# essentially just shifting the whole distribution
# over so that it's centered on 0 instead of on the
# group mean.

# Second, variances of the groups should be the same.
# If they aren't, the critical distance will be
# meaningless.

# Here's an example to prove my point.

Quantile <- seq(0, 27, 0.01)
Group_1 <- dnorm(Quantile, 10, 2)
Group_2 <- dnorm(Quantile, 17, 2)
Group_3 <- dnorm(Quantile, 19, 8)
plot(Group_1 ~ Quantile, type = 'l', col = 1, main = 'Looking for Differences\nBetween 3 Groups\nWhich Have Different Variances', xlab = 'Quantile', ylab = 'Density')
lines(Group_2 ~ Quantile, col = 2)
lines(Group_3 ~ Quantile, col = 3)
legend('topright', legend = paste("Group", 1:3), lty = 1, col = 1:3) #Does not meet the assumptions of ANOVA

# Since group 3 now has a much larger variance than
# the other two groups, the weighted-average variance
# will be greater than it was in the first example.
# Therefore, the critical distance will also be
# greater. Groups 1 and 2 probably won't be different
# anymore because of this new critical distance.
# Furthermore, groups 1 and 3 may be deemed different
# even though there is a significant amount of overlap
# between them, and this is because the critical
# distance is based on the weighted-average variance -
# this critical distance will be too big for the
# groups with the lower variances and too small for
# the group with the large variance.

# Let's work through an example.

Group_1 <- rnorm(5, 10, 2)
Group_2 <- rnorm(5, 17, 2)
Group_3 <- rnorm(5, 19, 2)
Response <- c(Group_1, Group_2, Group_3)
Group <- rep(paste0("Group_", 1:3), each = 5)
(My_Data <- data.frame(Group = Group, Response = Response))

# First, let's build the model.

# My_Data$Response ~ My_Data$Group

# Now, let's test the assumptions.

# Are the residuals normally-distributed?

(Residuals <- lm(Response ~ Group, data = My_Data)$resid)

# Let's look at a quantile-quantile plot. Allows us to test for normality

Mean <- mean(Residuals)
Standard_Deviation <- sd(Residuals)
Quantile <- seq(-5, 5, 0.01)
Frequency <- dnorm(Quantile, Mean, Standard_Deviation)
plot(Frequency ~ Quantile, type = 'l')

# Let's plot our points on this curve.

Our_Residuals <- dnorm(Residuals, Mean, Standard_Deviation)
points(Residuals, Our_Residuals, col = 2, pch = 19)

# Now, let's plot the theoretical values. Since we
# have 15 residuals, we'll have to split this curve up
# into 15 equally-sized regions by area and then
# calculate the midpoints of those regions (by area).
# The quantiles of those midpoints are our theoretical
# values.

#Split probability distrubution curve and find midpoints (by area)
for (i in 1:14) {
  segments(qnorm(i / 15, Mean, Standard_Deviation), 0, qnorm(i / 15, Mean, Standard_Deviation), dnorm(qnorm(i / 15, Mean, Standard_Deviation), Mean, Standard_Deviation))
}

# Now let's add the midpoints.

for (i in 1:15) {
  points(qnorm((2 * i - 1) / 30, Mean, Standard_Deviation), dnorm(qnorm((2 * i - 1) / 30, Mean, Standard_Deviation), Mean, Standard_Deviation), pch = 19, col = 3)
}

# Now, let's look at the quantile-quantile plot.

Theoretical_Quantiles <- qnorm((2 * 1:15 - 1) / 30, Mean, Standard_Deviation)
Residuals <- Residuals[order(Residuals)]
plot(Residuals ~ Theoretical_Quantiles)
abline(0, 1, col = 2)

# It looks pretty good. What do you think?

# We can test this assumption using the Shapiro-Wilk
# test. The null hypothesis of this test is that the
# data follow a normal distribution. Therefore, if the
# p value is greater than 0.05, we can say the data
# are normally-distributed.

shapiro.test(Residuals) #W is the Shapiro test statistic. Essentially correlatoin coefficient

# The Shapiro-Wilk test statistic (W) is very similar
# to, but not exactly the same as, the Pearson's
# correlation coefficient for the relationship
# between the residuals and these theoretical
# quantiles.

cor(Residuals, Theoretical_Quantiles)
shapiro.test(Residuals)$statistic

# Are the variances homogeneous?

# We can use Levene's test for this. The null
# hypothesis of this test is that variances are equal,
# so a p value that's greater than 0.05 suggests that
# variances are equal.

# There is a function to perform Levene's test but
# it's in the 'car' package, so we'll have to install
# and load that package.

if (!require (car)) {
  install.packages('car')
}
library (car)

leveneTest(Response ~ as.factor(Group), center = mean, data = My_Data) #Large p val suggests variances are equal

# How does Levene's test work?

# If variances are equal, the distance between
# individual data points and group means (or medians)
# should be, on average, the same across groups.

# All a Levene's test does is perform an analysis of
# variance on these distances (actually, it is on the
# absolute values of these differences).

My_Data_List <- split(My_Data, My_Data$Group)
Means <- lapply(My_Data_List, function (x) {
  mean(x$Response)
})
My_Data_List <- mapply(function (x, y) {
  x$Absolute_Distances <- abs(x$Response - y)
  return (x)
}, x = My_Data_List, y = Means, SIMPLIFY = F)
My_Data <- unsplit(My_Data_List, Group)

View(My_Data)#P val will equal levine test bc they are the same

summary(aov(Absolute_Distances ~ Group, My_Data)) 

# Our data meet both of these assumptions, so let's
# proceed with the analysis.

summary(aov(Response ~ Group, data = My_Data))

# It looks like there are statistically-significant
# differences between groups.

# If you want to learn more, please take Iago Hale's
# course, ANFS 993 (Design, Analysis, and
# Interpretation of Experiments) this spring.