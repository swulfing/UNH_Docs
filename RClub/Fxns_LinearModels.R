Nitrogen_Rate <- rep(c(0, 50, 100, 150, 200), 3)
Phosphorus_Rate <- rep(c(0, 50, 100), each = 5)
Corn_Yield <- c(44, 54, 55, 54, 52, 45, 67, 72, 75, 98, 33, 89, 91, 93, 88)

lm(mtcars$mpg ~ mtcars$cyl)
summary(lm(mtcars$mpg ~ mtcars$cyl))

#shows p value. Tests null hypothesis (slope or intercept = 0) if p < cutoff (i.e. 0.05) then we can reject null
#slope p value more important as a slope that doesn't = zzero means there is a relationship btwn variables

lm(mtcars$mpg ~ mtcars$cyl + mtcars$disp) #Addition sign isn't actually adding

#different ways to write
with(mtcars, lm(mpg~cyl + disp + hp))
lm(mpg~cyl + disp + hp, data = mtcars)

plot(mtcars$mpg ~ mtcars$cyl)
abline(lm(mtcars$mpg ~ mtcars$cyl), col = 2)

lm(mtcars$mpg ~ mtcars$cyl + (mtcars$cyl^2)) #carrot isn't actually raising to the power of two

lm(mtcars$mpg ~ mtcars$cyl + I(mtcars$cyl^2)) #Quadritic form


lm(mtcars$mpg ~ mtcars$cyl + mtcars$disp + mtcars$cyl:mtcars$disp)
#Gives slope for cylinder, gives slope for displacement AND slope for interaction (That is what the colon is)

lm(mtcars$mpg~mtcars$cyl * mtcars$disp)#exact same thing

#Three way interaction-Looks at the overall interaction, two way interactions and three way
lm(mtcars$mpg ~ mtcars$cyl * mtcars$disp * mtcars$hp)

lm(mtcars$mpg ~ (mtcars$cyl + mtcars$disp + mtcars$hp)^2) #gives you main effects and three two way interactions but omits three ways

#Adding a carrot 3 will do the same but omit any interactions larger than 3way

#polynomials
lm(mtcars$mpg ~ poly(mtcars$cyl, degree = 2))


Second_Order_Model <- lm(mpg ~ poly(cyl, degree = 2), data = mtcars)

Sequence_Of_Points <- with(mtcars, seq(min(cyl)), max(cyl), by = 0.01)
Predicted_Values <- predict(Second_Order_Model, newdata = data.frame(cyl = Sequence_Of_Points))

Predictor_Variable <- 1:100 + rnorm(100,0,0.5)
Response_Variable <- rnorm(1:100,0,0.5) + log(1:100)

My_Model <- lm(Response_Variable~Predictor_Variable)

#Use predict fxn to predict output of model

predict(My_Model, newdata = data.frame(Predictor_Variable = 55.5)) #thing in dataframe has to match old data

My_New_Model <- lm(mtcars$mpg ~ mtcars$cyl + mtcars$disp)

predict(My_New_Model, newdata - data.frame(cyl = 6, disp = 225))

lm(mtcars$mpg ~ mtcars$cyl / mtcars$disp)

Nitrogen_Rate <- rep(c(0, 50, 100, 150, 200), 3)
Phosphorus_Rate <- rep(c(0, 50, 100), each = 5)
Corn_Yield <- c(44, 54, 55, 54, 52, 45, 67, 72, 75, 98, 33, 89, 91, 93, 88)
interaction.plot(Nitrogen_Rate, Phosphorus_Rate, Corn_Yield) #Way to visualize treatments

#Effect of nitrogen rate is not consistent across phosphorus rates

interaction.plot(Phosphorus_Rate, Nitrogen_Rate, Corn_Yield)#swap
