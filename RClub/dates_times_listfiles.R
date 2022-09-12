#Dates and times
#Used with dataloggers often

Date_1 <- "04-05-2010"
str(Date_1)

#Want to store this numerically as a date to use it for graphing or mathematical fxns

as.Date(Date_1)
#Makes no sense because we didn't specify format

as.Date(Date_1, format = "%m-%d-%Y")

#Aways year, month, day. Uppercase y is a four digit year

Date_2 <- "10-4-20"
Date_3 <- "Oct. 4, 2020"

as.Date(Date_2, format = "%m-%d-%y")
as.Date(Date_3, format = "%b. %d, %Y")

#as.Date is vectorized meaning if I had a whole vector, we could format all of them in the same way

#Times

Time_1 <- "4-4-2010, 22:15:00"

#POSIct

as.POSIXct(Time_1)

as.POSIXct(Time_1, format = "%d-%m-%Y, %H:%M:%S")

Time_2 <- "10-3-2013 3:00:00 PM"
Time_3 <- "5:15 PM on Oct. 4th, 2020"

as.POSIXct(Time_2, format = "%m-%d-%Y %I:%M:%S %p")
as.POSIXct(Time_3, format = "%I:%M %p on %b. %d th, %Y")

as.POSIXct(Time_3, format = "%I:%M %p on %b. %d[:lower:]{2}, %Y")
as.POSIXct(Time_3, format = "%I:%M %p on %b. %d[:alpha:]{2}, %Y")

#Look into regular expressions


#list.files function returns all the names in a particular folder

# list.files("folderpath")
# Files <- file.info(list.files("folderpath"))

#Gives you lots of info on folder. Such as last time saved, last time changed, last time accessed

Vector_1 <- c("cat", "dog", "catdog", "horse")
Vector_2 <- c(5,6,3,4,2)

#Saves dates in POSIXct format so you can do the following operations on them

grep("cat", Vector_1) #positions of cat
grepl("cat", Vector_1) #T/F if contains cat

max(Vector_2)
which.max(Vector_2)
min(Vector_2)
which.min(Vector_2)

#Names of data loggers
Species <- c("Maple", "Birch", "Walnut")

All_Files <- (file.info(list.files("filepath", pattern = ".csv", full.names = TRUE)))#returns full names of csvs

Files_by_Species <- lapply(Species, function(x){grep(x, rownames(All_Files))})

#First list elemant is all maple elements, then birch then walnut

List_of_Files_to_Upload <- lapply(Files_by_Species, function(x){
  All_Files[x, ]
})
#Every time it loops it will apply the all file fxn to the position

List_of_Most_Recent_Files <- lapply(List_of_Files_to_Upload, function(x){
  x[which.max(x$mtime), ]
})
#Now this only includes the most recent of each type

Data_List <- lapply(List_of_Most_Recent_Files, function(x){read.csv(rownames(x))
})

















