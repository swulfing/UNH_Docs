observedData <- as.matrix(data.frame(read.csv("RaberinaryBenbow2012Calcs.csv")))
observedStageMatrix <- observedData[1:13, 2:5]

library(quadprog)
library(popbio)
#observedStageMatrix <- observedStageMatrix[2:11,]

#timepts <- sample(my_vec,10,replace=F)
my_vec = c(1:11,13)
my_vecs <- combn(my_vec,10)
lambda <- vector('numeric', length=ncol(my_vecs))
A_vec <- list()


for (trial in 1:ncol(my_vecs)){

#Make Z
z <- matrix(t(observedStageMatrix[c(my_vecs[,trial]),]), ncol = 1, nrow = 4*length(2:11), byrow = TRUE)
#z

#Make M
m <- matrix(, nrow = nrow(z) , ncol = 8)
for(i in 1:(nrow(observedStageMatrix)-3)){
  m[(4 * (i-1) + 1):(4 * i),] <- matrix(c(observedStageMatrix[i,1], 0, 0, 0, 0, 0, observedStageMatrix[i,4], 0,
                                          0, observedStageMatrix[i,1], observedStageMatrix[i,2], 0, 0, 0, 0, 0,
                                          0, 0, 0, observedStageMatrix[i,2], observedStageMatrix[i,3], 0, 0, 0,
                                          0, 0, 0, 0, 0, observedStageMatrix[i,3], 0, observedStageMatrix[i,4]),
                                        nrow = 4, byrow = TRUE)
  #m
}

#Make C
c <- matrix(c(diag(8) * (-1), 
              1, 1, 0, 0, 0, 0, 0, 0,
              0, 0, 1, 1, 0, 0, 0, 0,
              0, 0, 0, 0, 1, 1, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 1), nrow = 12, ncol = 8, byrow = TRUE)
#c
#Make b
b <- matrix(c(rep(0,8), rep(1,4)), nrow = 12, ncol = 1, byrow = TRUE)

#Make P
#p <- matrix(, nrow = 8, ncol = 1, byrow = TRUE) #vector(p1, g1, p2, g2, p3, g3, f4, p4)

#Make mtx G and vector x
g <- t(m) %*% m
f_t <- -(t(-z) %*% m)

qp <- solve.QP(Dmat = g, dvec = f_t, Amat = -t(c), bvec = -t(b))#, factorized = FALSE)


A <- matrix(c(qp$solution[1], 0, 0, qp$solution[7],
              qp$solution[2], qp$solution[3], 0, 0,
              0, qp$solution[4], qp$solution[5], 0,
              0, 0, qp$solution[6], qp$solution[8]), byrow = TRUE, nrow = 4, ncol = 4)
#A


A_vec[[trial]] <- A

lambda[trial] <- eigen.analysis(A)$lambda1

}


hist(lambda,breaks=15)
abline(v=mean(lambda))
abline(v=median(lambda))
range(lambda)
sort(lambda)
which(round(lambda,2)==round(0.9422901,2))
which(round(lambda,2)==round(1.0720191,2))


A<- A_vec[[53]]

