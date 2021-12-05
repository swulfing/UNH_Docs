# The final project should be done in groups of 1-3 people. The goal of the project is to put all of your skills to the test and have you work with some real data. 
# 
# You will need to submit a pdf and .Rmd of your write-up and do a short (5 minute) presentation on your findings.
# 
# The goal is to pull a dataset into R, clean the data, build 5+ figures/analyses/tables (you can choose), and build a nice pdf document. You also need to work on the project with your collaborators via Git and Github. I encourage you to work on data related to your research or interests, but this isn't 100% necessary. You can use any dataset you find online as well. Easton also has datasets you could work with if you aren't sure. Otherwise, the project details are fairly flexible.
# 
# Please upload the Github URL for your final project.




# Ideas:
#   Mapping of currents
#   Mtx models and projections (not based on this data)
#   Cost of oct over time/wt over time/No over time/avg wt over time
#   Sex of person catching. Then go into gendered convo

#Outline
#Intro place and population
#Go into Benbow study and that no one has modeled what that means for population stability
#show my model and all it's shortcomings/ all the assumptions I had to make (1 month per step, all 1 cohort, no immigration or emigration show map, stable population, only lookin at females) (maybe make a life history timeline)
#Compare to our dataset and add transform
# Equation with a weird name maybe and that it's telling us
#Future directions: What to fix model (what a stage based model looks like and which assumptions this eliminates), need to consider that just because something is at this weight doesn't mean it's at that stage (show benbow table) and eqn with weird name could fix this AND tell is if the pop is actually stable, if there is continuous spawning or diannual like the discourse I've read about

library(dplyr)
#library(modelr)
setwd("~/UNH_Docs/BIOL806/FinalProj")
CatchData <- read.csv("SalaryIIvilage2010_2016.csv")
CatchData <- data.frame(CatchData)

CephData <- CatchData %>% filter(Marine_product == "H")
#   Cost of oct over time
#   wt over time
#   No over time
#   avg wt over time
#   Sex of person catching. Then go into gendered convo

  # group_by(country) %>% #Doesn't quite do anything yet, just secretly stores as different data sets.
  # summarize(mean_pop = mean(pop), sd_pop = sd(pop))

CephData_dates <- CephData %>% group_by(Date)



#Mtx stuff
my_matrix <- matrix(c(0, 1.992, 0, 0, 0,
                      0, 0, 0.099, 0, 0,
                      .124*70000, 0, 0, .124, 0,
                      .25*70000, 0, 0, 0, .25,
                      0, 0, 0, 0, 0), ncol = 5)
print(my_matrix)

eigyBois <- eigen(my_matrix)
eigyBois

(values <- eigyBois$values)
(vectors <- eigyBois$vectors)

pop <- c(490, 976, 97, 12, 3)
pop1 <- my_matrix %*% pop
pop1
