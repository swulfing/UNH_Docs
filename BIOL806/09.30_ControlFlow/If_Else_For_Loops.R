# Control Flow Lab 

# Logical Statements 
2 == 2 
1 != 'dog' #! means not equal 
'cat'='dog'
2 | 1
'dog' = 'dog'

# If/else statements 
time <- 0
if(time == 1){
  print(time)
  }else if(time >1){
    print(time^2)
}else{
  print('time not equal to 1')}

head(cars)
cars$fastest <- ifelse(cars$speed <10,'slow','fast')


#for loops 
my_numbers <- c
for (index in 1:10){
  print(index^2)
  my_numbers <- c(my_numbers,index^2)
}
my_numbers <- vector(mode = 'numeric', 10)
my_numbers
my_numbers[index]<- index^2


#for loops and if/else
head(cars)
cars$fast_and_far <- 'NA'
for(row_num in 1:nrow(cars)){
  if(cars$speed[row_num]>15 & cars$dist[row_num]>50){
    cars$fast_and_far[row_num]<- 'fast and far'
  }else if (cars$speed <15 & cars$dist<50){
      cars$fast_and_far[row_num]<- 'slow and short'
  }else{
    cars$fast_and_far[row_num]<- 'neither'
  }
}
table(cars$fast_and_far)

LETTERS
for(index in LETTERS){
  if(index == 'A'){print(index)
  }else{
      print('not the letter A')
    }
}
