

# Matrices can be in any order. You can average eigenvalues or average matrices and then find eigenvalue

library(expm)  

A%^%2 
A%*%A


A2<- survivability(0.0001)


A%*%A%*%A %*% pop


A2%*%A%*%A %*% pop
A%*%A2%*%A %*% pop
A%*%A%*%A2 %*% pop

A%^%2 %*%A2 %*% pop



for (i in 1:11){
 print(sum(A%^%(12-i) %*%A2%^%(i) %*% pop)/sum(pop)) 
}

sum(A%^%11 %*%A2 %*% pop)/sum(pop)
sum(A%^%10 %*%A2%^%2 %*% pop)/sum(pop)
sum(A %*%A2%^%11 %*% pop)/sum(pop)


mean(c(as.numeric(eigen(A)$values[1]), as.numeric(eigen(A2)$values[1])))

eigen((A+A2)/2)


# Print eigenvalues for different combinations of matrices (A2 is increased survivability matrix)
for (i in 1:11){
  print(as.numeric(eigen(((12-i)*A+i*A2)/12)$values[1]))
}

