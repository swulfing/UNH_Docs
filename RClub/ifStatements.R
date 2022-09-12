x <- 5

if (x > 4){
  print("'x' is greater than 4.")
}

#Nesting if statements

if (x > 4){
  if (x < 8){
    print("'x' is between 4 and 8")
  }
}

#We can also combine this into one. logical "and" and "or", we know we can also write it like this:
if (x > 4 & x < 8){
  print("'x' is between 4 and 8") 
}

#ELSE statements
if (x > 4){
  print("'x' is greater than 4")
}  else if (x <= 4 & x > 2){
    print("'x' is less than or equal to 4 and greater than 2")
} else if (x <= 2){
    print("'x' is less than or equal to two")
  }

if (x > 4){
  print ("'x' is greater than 4")
} else{
  print("'x' is not greater than 4")
}

#Making an if statement on a vector. Right now gives an error that it will only work on one thing (first input)
x <- 1:10

#ifelse fxn is essentially a vectorized form of if statement ifelse(test, what to do with true, what to do with false)

help(ifelse) #two ways to look up library
?ifelse

(our_data <- data.frame(x = 1:10))
our_data$new_column <- ifelse(our_data$x > 5, "'x' is greater than 5", "'x' is not greater than 5")
our_data

#What if we have more than one eval metric?
our_data$another_new_column <-
  ifelse(our_data$x > 8, "'x' is greater than 8",
         ifelse(our_data$x <= 8 & our_data$x > 4, "'x' is less than or equal to 8 and greater than 4",
                ifelse(our_data$x <= 4 & our_data$x < 2, "'x' is less than or equal to 4 and greater than two",
                       "'x' is less than 2")))

#This is cumbersome so we use cut fxn cut(x, breaks, labels) breaks are cut offs and labels are what to do with cutoffs
?cut
our_data$one_last_column <- 
  cut(our_data$x, c(-Inf, 2, 4, 6, 8, Inf), #More cuts than labels
      c("Very Small", "Small", "Medium", "Large", "Very Large"))
our_data


#Looping- remember lapply and mapply. Looping is an alternative but the apply fxns are less computationally intensive
#3 types:

#For loops-loop through elements of a vector. Two ways to loop through object
# 1 loop through actual elements of a vector
# 2 loop through POSITIONS of a vector

#1
LETTERS
for(i in LETTERS){
  print(paste("The letter", i))
}

#2
LETTERS
for(i in 1:length(LETTERS)){
  print(paste("The letter", LETTERS[i]))
}


vector_1 <- round(rnorm(10), 2)

for (i in 1:length(vector_1)){
  print(paste("The ", i, "th element of vector_1 is ", 
              vector_1[i], sep = ""))
}

#seq_len is better than length. The only difference is how it handles an empty vector
1:0
seq_len(0)
vector_2 <-  NULL

for (i in 1:length(vector_2)){
  print(paste("The ", i, "th element of vector_2 is ", 
              vector_2[i], sep = ""))
}

for (i in 1:seq_len(length(vector_2))){
  print(paste("The ", i, "th element of vector_2 is ", 
              vector_2[i], sep = ""))
}

Baseball <- c("Boston Red Sox", "Toronto Blue Jays", "Tampa Bay Rays",
              "Baltimore Orioles", "New York Yankees")

ALL_POSSIBLE_MATCHUPS <- NULL
k <- 1

for (i in 1:(length(Baseball) - 1)){
  for(j in (i + 1):length(Baseball)){
    ALL_POSSIBLE_MATCHUPS[k] <- paste(Baseball[i],
                         "versus", Baseball[j])
    k <- k+1
  }
}
ALL_POSSIBLE_MATCHUPS










