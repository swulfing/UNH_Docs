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
library(ggplot2)
#library(modelr)
setwd("~/UNH_Docs/BIOL806/FinalProj")
CatchData <- read.csv("SalaryIIvilage2010_2016.csv")
CatchData <- data.frame(CatchData)

CatchData$Sex <- as.character(CatchData$Sex)
CatchData$Date <- as.Date(CatchData$Date, format = "%d-%b-%y")
CatchData$Number_of_individuals <- as.numeric(CatchData$Number_of_individuals)
CatchData$Kilos <- as.numeric(CatchData$Kilos)
CatchData$Average_Size <- as.numeric(CatchData$Average_Size)
CatchData$USD_Price <- as.numeric(CatchData$USD_Price)
CephData <- CatchData %>% filter(Marine_product == "H")

#   Cost of oct over time
#   wt over time
#   No over time
#   avg wt over time
#   Sex of person catching. Then go into gendered convo

  # group_by(country) %>% #Doesn't quite do anything yet, just secretly stores as different data sets.
  # summarize(mean_pop = mean(pop), sd_pop = sd(pop))


CephData_dates <- CephData %>% 
  select(Date, Age, Sex, Number_of_individuals, Kilos, Average_Size, Price_Ar, USD_Price, When_returned_to_shore, Corresponding_time_interval_for_Arrival_time) %>%
  group_by(Date) %>%
  summarise(mean_number = mean(Number_of_individuals, na.rm=TRUE), mean_wt = mean(Kilos, na.rm=TRUE), mean_size = mean(Average_Size, na.rm=TRUE), mean_price = mean(USD_Price, na.rm=TRUE))
CephData_dates  
# summarize(mean_price = mean(Price_Ar), mean_wt_tot = mean(Kilos), 
  #           mean_count = mean(Number_of_individuals), mean_avg_size = mean(Average_Size))

ggplot(data = CephData_dates, aes(x = Date, y = mean_number)) +
  geom_point(color = "black") +
  geom_smooth(method="glm") +
  ylab("No. of Individuals")

ggplot(data = CephData_dates, aes(x = Date, y = mean_wt)) +
  geom_point(color = "black") +
  geom_smooth(method="glm") +
  ylab("Weight")

ggplot(data = CephData_dates, aes(x = Date, y = mean_price)) +
  geom_point(color = "black") +
  geom_smooth(method="glm") +
  ylab("Price (USD)")

CephData_sex <- CephData %>% 
  select(Date, Sex, Age, Number_of_individuals, Kilos, Average_Size, Price_Ar, USD_Price, When_returned_to_shore, Corresponding_time_interval_for_Arrival_time) %>%
  filter(Sex == "L" | Sex == "A") %>%
  mutate(Sex = replace(Sex, Sex == "L", "Male")) %>%
  mutate(Sex = replace(Sex, Sex == "A", "Female"))

ggplot(CephData_sex, aes(x=Sex, y=Kilos, fill = Sex)) + 
  geom_boxplot() +
  scale_fill_brewer(palette="Paired") +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(legend.position="none")

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
N <- list()
 
for (i in 1:6){
   N[[1]] <- pop
   N[[i+1]] <-my_matrix %*% N[[i]]
 }

modeled_data <- t(as.data.frame(do.call(cbind, N)))
colnames(modeled_data) <- c("Stage_1", "Stage_2", "Stage_3", "Stage_4", "Stage_5")
modeled_data <- cbind(modeled_data, month = 0:6)

write.csv(modeled_data, "testing.csv",row.names = TRUE)
modeled_data <- data.frame(modeled_data)

ggplot(modeled_data) + 
  geom_line(aes(y = Stage_1, x = month, color = "Stage 1"), size = 1.5) + 
  geom_line(aes(y = Stage_2, x = month, color= "Stage 2"), size = 1.5) +
  geom_line(aes(y = Stage_3, x = month, color = "Stage 3"), size = 1.5) + 
  geom_line(aes(y = Stage_4, x = month, color= "Stage 4"), size = 1.5) +
  geom_line(aes(y = Stage_5, x = month, color = "Stage 5"), size = 1.5) +
  scale_color_manual(name = "", values = c("Stage 1" = "#009E73",
                                                "Stage 2" = "#F0E442",
                                                "Stage 3" = "#0072B2",
                                                "Stage 4" = "#D55E00",
                                                "Stage 5" = "#CC79A7")) +
  xlab("Month") +
  ylab("No. Individuals")
  

