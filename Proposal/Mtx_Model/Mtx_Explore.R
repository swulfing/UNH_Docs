M <- matrix(c(0,0,0,0,127,4,80,
              0.6747,0.7370,0,0,0,0,0,
              0,0.0486,0.6610,0,0,0,0,
              0,0,0.0147,0.6907,0,0,0,
              0,0,0,0.0518,0,0,0,
              0,0,0,0,0.8091,0,0,
              0,0,0,0,0,0.8091,0.8089), nrow = 7, byrow = TRUE)
print(M)

eigyBois <- eigen(M)

# extract components
(values <- eigyBois$values)
(vectors <- eigyBois$vectors)

rvals <- log(values)

