# Vectorization: not using for loops for it. 
# 
# eg. y <- mean (x,...) with ... being options e.g. na.rm = true
# y is output, x is input. fxn name is "mean" in this case
# 
# X=some matrix with cols a-d and rows 1-10
# mean(x) would give mean of all of the mtx
# mean(x$b)would only take mean of b
# mean(subset(x, x$b>5))
# 
# take mean of each column: could do each individually, could do for loop OR vectorization
# e.g. there's a fxn called colMeans(x) = c(3,4,5,5,...). Output will be a list
# 
# How to build fxns in r:
# 
# Farenheit_to_celsuis <- function(x){
#   celsuis <- (x-32)*(5/9)
#   return (celsius)
# }
# 
# In this case x is a number. put in vector, fxn will automatically revectorize

fahr_to_celsius <- function(x){
  celsius <- (x - 32)*(5/9)
  return(celsius)
}

#If drawing on a plot, don't need a return command. but can have one
plot(0,0)
add_linear_line <- function(intercept, slope){
  abline(coef=c(intercept, slope))
}
add_linear_line(0,.5)



