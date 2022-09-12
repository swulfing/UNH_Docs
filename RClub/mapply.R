library(dplyr)

Year1Data <- data.frame(Depth_cm = rep(c("0-10", "10-20", "20-40"), 3),
                        Site = rep(c("Forest", "Pasture", "Corn_Field"), each = 3),
                        carbon_mg_per_kg = rnorm(9, 18, 5),
                        nitrogen_mg_per_kg = rnorm(9, 4.8, 0.5),
                        Year = 1)
Year2Data <- data.frame(Depth_cm = rep(c("0-10", "10-20", "20-40"), 3),
                        Site = rep(c("Forest", "Pasture", "Corn_Field"), each = 3),
                        carbon_mg_per_kg = rnorm(9, 19, 5),
                        nitrogen_mg_per_kg = rnorm(9, 5, 0.5),
                        Year = 2)
Year3Data <- data.frame(Depth_cm = rep(c("0-10", "10-20", "20-40"), 3),
                        Site = rep(c("Forest", "Pasture", "Corn_Field"), each = 3),
                        carbon_mg_per_kg = rnorm(9, 20, 5),
                        nitrogen_mg_per_kg = rnorm(9, 5.2, 0.5),
                        Year = 3)
Year4Data <- data.frame(Depth_cm = rep(c("0-10", "10-20", "20-40"), 3),
                        Site = rep(c("Forest", "Pasture", "Corn_Field"), each = 3),
                        carbon_mg_per_kg = rnorm(9, 18, 2.5),
                        nitrogen_mg_per_kg = rnorm(9, 4.8, 0.25),
                        Year = 4)
Year5Data <- data.frame(Depth_cm = rep(c("0-10", "10-20", "20-40"), 3),
                        Site = rep(c("Forest", "Pasture", "Corn_Field"), each = 3),
                        carbon_mg_per_kg = rnorm(9, 20, 5),
                        nitrogen_mg_per_kg = rnorm(9, 5.2, 0.5),
                        Year = 5)
Year6Data <- data.frame(Depth_cm = rep(c("0-10", "10-20", "20-40"), 3),
                        Site = rep(c("Forest", "Pasture", "Corn_Field"), each = 3),
                        carbon_mg_per_kg = rnorm(9, 17, 3.5),
                        nitrogen_mg_per_kg = rnorm(9, 5.5, 0.5),
                        Year = 6)
Year7Data <- data.frame(Depth_cm = rep(c("0-10", "10-20", "20-40"), 3),
                        Site = rep(c("Forest", "Pasture", "Corn_Field"), each = 3),
                        carbon_mg_per_kg = rnorm(9, 20, 4.5),
                        nitrogen_mg_per_kg = rnorm(9, 5.3, 0.45),
                        Year = 3)
Year8Data <- data.frame(Depth_cm = rep(c("0-10", "10-20", "20-40"), 3),
                        Site = rep(c("Forest", "Pasture", "Corn_Field"), each = 3),
                        carbon_mg_per_kg = rnorm(9, 22, 5.5),
                        nitrogen_mg_per_kg = rnorm(9, 4.8, 0.4),
                        Year = 3)
Year9Data <- data.frame(Depth_cm = rep(c("0-10", "10-20", "20-40"), 3),
                        Site = rep(c("Forest", "Pasture", "Corn_Field"), each = 3),
                        carbon_mg_per_kg = rnorm(9, 21, 5),
                        nitrogen_mg_per_kg = rnorm(9, 4.9, 0.5),
                        Year = 9)
Corn_Field <- data.frame(phosphorus_mg_per_kg = 1.8, potassium_mg_per_kg = 2.3)
Forest <- data.frame(phosphorus_mg_per_kg = 0.9, potassium_mg_per_kg = 2.1)
Pasture <- data.frame(phosphorus_mg_per_kg = 1.2, potassium_mg_per_kg = 1.9)
(Metadata <- list(Corn_Field = Corn_Field, Forest = Forest, Pasture = Pasture))

#lapply recap
#l stands for list. mapply, the m stands for multivariate

#Named list
#MyData <- list(year1 = Year1Data, year2 = Year2Data, year3 = Year3Data)

#Unnamed list, note the square brackets
MyData <- list(Year1Data, Year2Data, Year3Data)

#C to N ratios
#Step 1 practice on a single list element or dataframe
#Step 2 write a fxn with the stuff from step 1 in the body of the fxn
#Step 3 apply new fxn to each list element (across whole list)

#putting things in parenthesis will write it directly in console
(Year1Data$C_to_N_Ratio <- Year1Data$carbon_mg_per_kg / Year1Data$nitrogen_mg_per_kg)

my_fxn <- function (x) {
  x$C_to_N_Ratio <- x$carbon_mg_per_kg / x$nitrogen_mg_per_kg
  return(x)
  }
#If you forget return, the next step will only apply the fxn to the first dataframe
lapply(MyData, my_fxn)

#Now calculate C to P ratios. Phosphorus only measured once in Metadata list. We have it at site level, not year level. What do we do? First, use rowbind. note: if you run R as is, Year 1 has and extra column because we added the C:N ratio
NewDF <- rbind(Year1Data, Year2Data, Year3Data)
View(NewDF)

#splits up a df into a list of dfs by some factor (column in the df), in this case site
NewList <- split(NewDF, NewDF$Site)

#Columns need to have the same titles in the same order, so check it. If not, check out the sort and order functions
names(NewList)
names(Metadata)

##### mapply timeeeeeeeeee #####
#same steps as before
#step 1 practice on 1st list element from each list
Practice_DF1 <- NewList[[1]]
Practice_DF2 <- Metadata[[1]]

#Create C to P ratios
#Step 1
#Recycling. when you have two vectors of different lenths, it reuses the short one until the long one is used up
Practice_DF <- Practice_DF1$carbon_mg_per_kg/Practice_DF2$phosphorus_mg_per_kg

#Step 2 - create our function, combining info from TWO lists
#We will write a function of 2 variables
#first will represent those with C and N values
#second will represent those with P and K values

fxn1 <- function(CNdata, PKdata){
  CNdata$C_to_P_Ratio <- CNdata$carbon_mg_per_kg/PKdata$phosphorus_mg_per_kg
  return(CNdata)
  }
#step 2b. test fxn on practice data frames
fxn1(Practice_DF1, Practice_DF2)

#step 3 apply function across lists
#mapply will try to simpify arguments into types of data. put false to maintain types of list
mapply(fxn1, CNdata = NewList, PKdata = Metadata, SIMPLIFY = FALSE )


#Nested lists, lists of lists. In order to use lapply and mapply functions, you nee to nest lapply and mapply as well.

#lapply
NestedList1 <- list(element1 = list(Year1Data, Year2Data, Year3Data), 
                    element2 = list(Year4Data, Year5Data, Year6Data),
                    element3 = list(Year7Data, Year8Data, Year9Data))

#str looks at the structure of lists
str(NestedList1)

#Create C to N ratios
my_fxn <- function (x) {
  x$C_to_N_Ratio <- x$carbon_mg_per_kg / x$nitrogen_mg_per_kg
  return(x)
}

lapply(NestedList1, function(x){
  lapply(x, nrow)
})

lapply(NestedList1, function(x){
  lapply(x, colnames)
})

### Copied from class notes###
lapply(NestedList1, function (x) {
  lapply(x, function (y) {
    y$C_to_N_Ratio <- y$carbon_mg_per_kg / y$nitrogen_mg_per_kg
    return (y)
  })
})

# The final challenge:

# What if we have two nested lists that we want to perform
# operations on? For example:

NestedList1
MetadataNested <- list(Metadata, Metadata, Metadata)

# Can you figure out how to use the 'mapply' function to
# create a 'C_to_P_Ratio' column in each of the data
# frames? This operation will be similar to the one we
# did previously, but now we are working with one more
# level of nesting than we were previously.

# Answer:

fxn2 <- function (x, y) {
  x$C_to_P_Ratio <- x$carbon_mg_per_kg / y$phosphorus_mg_per_kg
  return (x)
}

mapply(function (a, b) {
  mapply(fxn2, x = a, y = b, SIMPLIFY = F)
}, a = NestedList1, b = MetadataNested, SIMPLIFY = F)





