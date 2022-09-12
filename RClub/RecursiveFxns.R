## Recursive Fxns ## 

Function_1 <- function(x){
  if (x == 1){
    warning("'x' is equal to to 1 and it won't change when you square it")
  }
  x^2
}

Function_1(2)
Function_1(1)

#Error Messafges-Same. Doesn't just run it shuts down

Function_2 <- function(x){
  if (x == 0){
    stop("You can't divide by zer0")
  }
  1/x
}

Function_2(0)


#Can use double quites inside single quotes or vise versa

#Recursive fxn is a fxn that calls itself
Fib_Seq_Fxn <- function(x){
  if((x %% 1 != 0) | (x < 1) | (length(x) != 1) | (!is.numeric(x))){
    stop ("'x' is not a postitive int")}
  if (x ==1){
    return (0)
  } else if (x ==2){
    return(1)
  } else if (x > 2)
    return(Fib_Seq_Fxn(x-1) + Fib_Seq_Fxn(x-2))
}

Fib_Seq_Fxn(1)
Fib_Seq_Fxn(2)
Fib_Seq_Fxn(3)
Fib_Seq_Fxn(4)
Fib_Seq_Fxn(5)
Fib_Seq_Fxn(10)

#Research ex
Data_Frame_1 <- data.frame(Column_1 = LETTERS[1:10], Column_2 = rnorm(10))
Data_Frame_2 <- data.frame(Column_1 = LETTERS[1:10], Column_2 = rnorm(10))
Data_Frame_3 <- data.frame(Column_1 = LETTERS[1:10], Column_2 = rnorm(10))
Data_Frame_4 <- data.frame(Column_1 = LETTERS[1:10], Column_2 = rnorm(10))
Data_Frame_5 <- data.frame(Column_1 = LETTERS[1:10], Column_2 = rnorm(10))
Data_Frame_6 <- data.frame(Column_1 = LETTERS[1:10], Column_2 = rnorm(10))
Data_Frame_7 <- data.frame(Column_1 = LETTERS[1:10], Column_2 = rnorm(10))
Data_Frame_8 <- data.frame(Column_1 = LETTERS[1:10], Column_2 = rnorm(10))
Data_Frame_9 <- data.frame(Column_1 = LETTERS[1:10], Column_2 = rnorm(10))
Nested_List_1 <- list(Data_Frame_1, Data_Frame_2, Data_Frame_3)
Nested_List_2 <- list(list(Data_Frame_1, Data_Frame_2, Data_Frame_3), list(Data_Frame_4, Data_Frame_5, Data_Frame_6), list(Data_Frame_7, Data_Frame_8, Data_Frame_9))
str(Nested_List_2)
class(Nested_List_2)

Recursive_Fxn <- function(x){
  if(!(class(x) %in% c("data.frame", "list"))){
    stop("'x' is not a dataframe or a list")
  }
  if(class(x) == "list"){
    lapply(x, Recursive_Fxn)
  } else if (class(x) == "data.frame") {
    x$Column_3 <- x$Column_2 ^2
    return (x)
  }
}

Recursive_Fxn(Nested_List_1)
Recursive_Fxn(Nested_List_2)












