
# tree sample -------------------------------------------------------------

# Create fake dataset
# White pines in college woods
# Suppose there are 567 pines
# True mean is 21.6 m, True St. Dev is 4.6
# We will assume the heights are normally distributed

set.seed(42) #Pseudo random number generator in R


# Population
pines <- rnorm(n = 567, mean = 21.6, sd = 4.6) #rnorm is normal dist. BaseR also has other distributions you can pick from
t_mean <- 21.6
t_sd <- 4.6
t_median <- median(pines)
t_range <- max(pines) - min(pines)

# Samples
pines_s <- sample(x = pines, 
                  size = 25,
                  replace = FALSE)
#sample params                  
s_mean <- mean(pines_s)
s_sd <- sd(pines_s)
s_median <- median(pines_s)
s_range <- max(pines_s) - min(pines_s)

#Put all of the above into a table
t <- data.frame(dataset = c("true", "sample"),
                mean = c(t_mean, s_mean),
                sd = c(t_sd, s_sd),
                median = c(t_median,s_median),
                range = c(t_range, s_range)
                )


#Want to report range of covariates bc range of sample will kind of predict what you can predict of the true sample. Dont want to extrapolate

choose(567, 25) #Number of possible 25 sample tree combos

# Create a loop to repeatedly take random samples, calc mean, and store it
pines_sample_means <- NA

for(i in 1:40000){
  pines_s <- sample(pines, 25, FALSE)
  
  pines_sample_means[i] <- mean(pines_s)
}

# Is pines_sample_means normal?
hist(pines_sample_means, col = "wheat")



# dice rolling ------------------------------------------------------------

# ctrl shift r will create different sections. Top left button with dashes is a directory of setions
values_1 <- c(6, 2, 6, 1, 5, 1, 2, 2, 6, 
              6, 5, 1, 2, 6, 6, 3, 6, 5, 
              2, 5, 4, 5, 1, 1, 2, 4, 4,
              3, 3, 2, 2, 4, 3, 6, 1)
hist(values_1, breaks = c(0:5)) #This is binning weird
stem(values_1)


values_2 <- c(8, 7, 5, 10, 9, 6, 8, 6, 10,
              9, 7, 2, 8, 4, 9, 6, 10, 8,
              7, 7, 8, 11, 9, 4, 9, 9, 8,
              9, 9, 5, 10, 8, 7, 11, 10,
              2, 8, 3)
hist(values_2)
# Normal distributions tend to arise from ADDITIVE PROCESSES

values_3 <- c(4,4,3,8,6,2,20,15,12,18,15,
              12,20,15,2,6,20,6,20,25,30,
              6,15,36,36,16,9,12,2,30)
hist(values_3)




