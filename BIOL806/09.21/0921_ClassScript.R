cats <- data.frame(coat = c("calico", "black", "tabby"),
                   weight = c(2.1, 5.0, 3.2),
                   likes_string = c(1, 0, 1))
write.csv(x = cats, file = "09.21/data/feline-data.csv", row.names = FALSE)

head(cats)
dim(cats)
View(cats)
cats[1,] #top row
cats[,1] #first column
cats[1,1]
cats[1:2,] #show rows 1 and 2

cats[c(1,3),] #first and 3rd row, all columns



#subsetting data
cats$weight < 5 #Runs for whole column, comes back with logic statements
cats[cats$weight < 5,] #Returns Rows that 
cats[cats$coat == "calico",] # == is for logic statements

cats_calico <- cats[cats$coat == "calico",] #Make a subset of data

#Alt minus is shortcut for <- 

y <- 1:10
class(y) #Will tell you data type

class(cats$coat)
typeof(cats$coat) 

class(cats$likes_string) #from human POV
typeof(cats$likes_string) #data type from R's perspective

class(cats) #dataframe
typeof(cats) #list

x <- vector(mode = "numeric", length = 10)
x[9] = "cat"
x #R changes data type to most flexible, so it will force string
as.numeric(x) #Command to convert to numeric. Forcing to be int will result in NA


z <- list("cats", 5, TRUE) #No longer a vector, more of a series of vectors
z

#mean(z) Doesn't know how to interpret this

cats[cats$weight != 5.0,]
cats[cats$weight != 5.0, "coat"] #only include rows that aren't 5 but only display coat col
cats[cats$weight != 5.0, c("coat", "likes_string")]

#==, !=, <, <=, >, >=, | (veritcal bar means "or"), &


#Pull data into R

cats <- read.csv("09.21/data/feline-data.csv", 
                 header = TRUE, 
                 stringsAsFactors = TRUE) #pusing tab after writing data will finish line for you
class(cats)
class(cats$coat) #Now its a factor, not a string!!!
typeof(cats$coat) #Says integer. Why?

#string is a group of character. With factor, R assigns numbers to each string. 

cats$happy <- c("yes", "no", "no")


paste(2, "me", sep = "-")
cats$happy_string <- paste (cats$happy, cats$likes_string, sep = "_")
cats

strsplit(cats$happy_string, split = "_")

cats$sophie <- cats$weight * cats$likes_string

cats$weight <- cats$weight*2.2#replaces column with new info
cats

#matrices same as dataframe, except everything is one type of data
my_mat <- matrix(c(1:4), 
                 nrow = 2, 
                 ncol = 2)
#my_mat [2,2] = "me" like dataframes, this will convert everything to strigns
my_mat
my_mat*2
my_mat[2,2]
mean(my_mat)
