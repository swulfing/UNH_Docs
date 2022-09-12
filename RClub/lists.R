Example_1 <- list(AAA = 3, BBB = 'cat', CCC = 1:10, DDD = LETTERS, EEE = matrix(1:24, ncol = 4), FFF = mtcars, GGG = list(obj1 = iris, obj2 = 4 , obj3 = state.abb), HHH = summary(lm(iris$Sepal.Length ~ iris$Petal.Length)))
Example_2 <- with(mtcars, lm(hp ~ mpg))
Example_3 <- list(Year1Data = data.frame(Depth_cm = rep(c("0-10", "10-20", "20-40"), 3), Site = rep(c("Forest", "Pasture", "Corn_Field"), each = 3), carbon_mg_per_kg = rnorm(9, 18, 5), nitrogen_mg_per_kg = rnorm(9, 4.8, 0.5), Year = 1), Year2Data = data.frame(Depth_cm = rep(c("0-10", "10-20", "20-40"), 3), Site = rep(c("Forest", "Pasture", "Corn_Field"), each = 3), carbon_mg_per_kg = rnorm(9, 19, 5), nitrogen_mg_per_kg = rnorm(9, 5, 0.5), Year = 2), Year3Data = data.frame(Depth_cm = rep(c("0-10", "10-20", "20-40"), 3), Site = rep(c("Forest", "Pasture", "Corn_Field"), each = 3), carbon_mg_per_kg = rnorm(9, 20, 5), nitrogen_mg_per_kg = rnorm(9, 5.2, 0.5), Year = 3))
Example_4 <- list(Corn_Field = data.frame(phosphorus_mg_per_kg = 1.8, potassium_mg_per_kg = 2.3), Forest = data.frame(phosphorus_mg_per_kg = 0.9, potassium_mg_per_kg = 2.1), Pasture = data.frame(phosphorus_mg_per_kg = 1.2, potassium_mg_per_kg = 1.9))
Example_5 <- lapply(split(do.call('rbind', Example_3), do.call('rbind', Example_3)$Site), function (x) {split(x, x$Depth_cm)})
Example_6 <- lapply(Example_5, function (x) {lapply(x, function (y) {data.frame(phosphorus_mg_per_kg = rnorm(1, 2, 0.2), potassium_mg_per_kg = rnorm(1, 3, 0.3))})})
Example_7 <- list(A = 2, B = list(1:10, 11:20, list('a', 'b', list(1, 1:10))), 'qqq', matrix(1:60, ncol = 6))
Example_8 <- list(a = c(1, 3, 5), b = c(2,4,6), c = c(9,8,7))
Example_9 <- matrix(1:24, ncol = 4)
Example_10 <- array(1:100, dim = c(5, 5, 4))
Example_11 <- structure(list(price = c(21, 32, 46, 19, 29, 31, 37, 28, 50, 29, 34, 40, 26, 20, 48, 7, 39, 30, 40, 34, 51, 18, 46, 50, 30), type = structure(c(2L, 3L, 4L, 3L, 1L, 3L, 4L, 1L, 1L, 3L, 1L, 2L, 4L, 4L, 1L, 4L, 2L, 1L, 4L, 2L, 1L, 2L, 1L, 3L, 2L), levels = c("toy", "food", "electronics", "drinks"), class = "factor"), store = c("Store 2", "Store 3", "Store 4", "Store 4", "Store 4", "Store 4", "Store 1", "Store 2", "Store 3", "Store 2", "Store 2", "Store 2", "Store 4", "Store 3", "Store 3", "Store 1", "Store 3", "Store 4", "Store 2", "Store 1", "Store 4", "Store 1", "Store 1", "Store 1", "Store 2"), type1 = structure(c(2L, 3L, 4L, 3L, 1L, 3L, 4L, 1L, 1L, 3L, 1L, 2L, 4L, 4L, 1L, 4L, 2L, 1L, 4L, 2L, 1L, 2L, 1L, 3L, 2L), levels = c("toy", "food", "electronics", "drinks"), class = "factor")), row.names = c(NA, -25L), class = "data.frame")

Example_1
str(Example_1)

str(Example_2)
str(Example_2[[2]])
str(Example_2[2])
Example_2[1:3]

#Using names
Example_2$coefficients
Example_2["coefficients"] #Same thing as the double/single bracket thing

#This is a nested list. Can use any combo of names/numbers to get nests in lists

# laaply stuff
lapply(Example_1, class)
lapply(Example_1, length)
lapply(Example_1, name)

#CCreate a carbon to nitrogen ratio column
Example_3 #list with three list elements where each is a dataframe
#Each structured the same way

#Now make a ratio column - use lapply on all three dataframes all at once
practice <- Example_3[[1]]
practice$CtoNRatio <- practice$carbon_mg_per_kg/ practice$nitrogen_mg_per_kg


fxn1 <- function(x){
  (x$CtoNRatio <- x$carbon_mg_per_kg/ x$nitrogen_mg_per_kg)
  x
  
}

fxn1(Example_3[[3]])

#important that all have same colnames

lapply(Example_3, fxn1) #can also write the function directly into lapply

Example_3 <- lapply(Example_3,function(x){
  (x$CtoNRatio <- x$carbon_mg_per_kg/ x$nitrogen_mg_per_kg)
  x
  
})

#mapply. the l in lapply stands for list. m here stands for multivariate
Example_4
Example_3

#4 is organized by site, the other by year
new <- do.call('rbind', Example_3)
newer <- split(new, new$Site)

names(Example_4) #THESE TWO NEED TO BE IN THE SAME ORDER OR ELSE IT WON'T WORK
names(newer)

practice1 <- newer[[1]]
practice2 <- Example_4[[1]]

practice1$C_to_P_Ratio <- practice1$carbon_mg_per_kg / practice2$phosphorus_mg_per_kg

fxn2 <- function(x,y){
  x$C_to_P_Ratio <- x$carbon_mg_per_kg / y$phosphorus_mg_per_kg
  x
}

(fxn2(newer[[2]], Example_4[[2]]))

#Nested lists
Example_5
Example_6

lapply(Example_5, function(x){
  lapply(x, function(y){
    y$CtoNRatio <- y$carbon_mg_per_kg/ y$nitrogen_mg_per_kg
    y
  })
})

# Creating a phosphorus to nitrogen ratio with nested list
mapply(function(x,y){
  mapply(function(p,q){
    p$C_to_P_Ratio <- p$carbon_mg_per_kg / q$phosphorus_mg_per_kg
  p
}, p = x, q = y, SIMPLIFY = F)
}, x = Example_5, y = Example_6, SIMPLIFY = F)

#rapply function. r stands for recursive
Example_7
str(Example_7) #some elements arent lists but others are

rapply(Example_7, length) #Digs into so until its no longer a list
rapply(Example_7, class)

#sapply s stans for simplify
Example_8
sapply(Example_8, max) #returns a vector because each list has only one element
lapply(Example_8, max) #returns a list

#vapply stands for vector
Example_8$d <- c('a', 'b', 'f')
sapply(Example_8, max)
vapply(Example_8, max, numeric(1)) #throws an error if non numeric elements

#apply
Example_9 #matric, not a list
apply(Example_9, 1, sum) #1 means first dimension i.e. rows
apply(Example_9, 2, mean) #2 emans second dimension i.e. columns

Example_10 #3d array
apply(Example_10, 1, sum)
apply(Example_10, 2, mean)
apply(Example_10, 3, max)
apply(Example_10, c(1,2), max)#do it on two dimenstions

#tapply
Example_11$type <- factor(Example_11$type, labels = c("toy", "food", "electronics", "drinks"))

mean_prices <- tapply(Example_11$price, Example_11$type, mean) #second element has to be a factor for tapply
max_prices <- tapply(Example_11$price, Example_11$type, max)
