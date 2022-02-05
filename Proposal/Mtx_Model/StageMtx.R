setwd("C:/Users/sophi/Documents/UNH_Docs/Proposal/Mtx_Model")

library(matlib)

observedStageMatrix <- as.matrix(data.frame(read.csv("RaberinaryBenbow2012Calcs.csv")))
#Mtx where T1 = Feb 2005 

#In 6.2.1 Lefkovitch Regression method
#Starting w T = 1-4 then will go on to calculated other T's and perform smoothing (hopefully)

#Stage 1 start
# b1 <- as.vector(observedStageMatrix[2:5,2])
# A1 <- observedStageMatrix[c(1:4), c(2:5)] #as.matrix(observedStageMatrix[1:4, 5], observedStageMatrix[1:4, 2], ncol = 2)
# X1 <- inv(A1) %*% b1
# 
# F1 <- X1[1]
# P1 <- X1[2]

# calcdStageMatrix <- matrix(, nrow = 4, ncol = 4)
# A <- observedStageMatrix[c(1:4), c(2:5)] #as.matrix(observedStageMatrix[1:4, 5], observedStageMatrix[1:4, 2], ncol = 2)

# 
# 
# 
# for(j in 1:nrow(calcdStageMatrix)){
# 
#   b <-as.vector(observedStageMatrix[2:5, j + 1])
#   X <- inv(A) %*% b
#   
#   calcdStageMatrix[j,] <- X#calcdStageMatrix[X,] <- vector
#   
# }


for (i in 1:(nrow(observedStageMatrix)-5)){

  calcdStageMatrix <- matrix(, nrow = 4, ncol = 4)
  A <- observedStageMatrix[c(i:(i+3)), c(2:5)]
  
  for(j in 1:nrow(calcdStageMatrix)){
    
    b <-as.vector(observedStageMatrix[2:5, j + 1])
    X <- inv(A) %*% b
    
    calcdStageMatrix[j,] <- X
    
  }  
  assign(paste0("calcdStageMatrix", i), calcdStageMatrix)
}


for(i in 1:nrow(calcdStageMatrix)){
  
  for( j in 1:ncol(calcdStageMatrix)){
    if(calcdStageMatrix[i,j] < 0) {
      calcdStageMatrix[i,j] <- 0
      
    }
  }
}
calcdStageMatrix
