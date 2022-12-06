library(dplyr)

setwd("C:/Users/sophi/Documents/GitHub/UNH_Docs")

all_data <- read.csv("ModelClassData.csv")

sp <- all_data %>%
  count(Fish.ID)
sp

data <- all_data %>%
  filter(Fish.ID == "french grunt (Haemulon flavolineatum)")
