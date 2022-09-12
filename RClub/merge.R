## UNH r users club code on projects and merge function
## Brittany jellison - 07/21/22

rm(list=ls())

############################################################################################################################
## load packages
############################################################################################################################
##pacman can install packages that aren't already installed.

library(pacman)
p_load(tidyverse,ggpmisc,broom,forcats)

############################################################################################################################
## create dataframes
############################################################################################################################

tank_id <- 1:10
mussel_tag <- c("blue", "green", "yellow", "red", "white",
                   "purple", "brown", "black", "pink", "beige")
mussel_length <- round(rnorm(10, mean = 15, sd = 2))
mussel_weight <- round(rnorm(10, mean = 4, sd = 1))
temperature_treatment <- c("low", "medium", "high", rep("extreme", 7))


d_1 <- data.frame(tank_id = tank_id[1:8], tag = mussel_tag[1:8],
                   mussel_length = mussel_length[1:8], mussel_weight = mussel_weight[1:8])
d_2 <- data.frame(tank_id = tank_id[-5], tag = mussel_tag[-5], temperature_treatment = temperature_treatment[-5])

d_1
d_2

############################################################################################################################
## save dataframes
############################################################################################################################

write.csv(d_1, "data/mussel data.csv", row.names=F)
write.csv(d_2, "data/mussel treatment.csv", row.names=F)

############################################################################################################################
## load dataframes
############################################################################################################################

d_1 <- read.csv("data/mussel data.csv")
d_2 <- read.csv("data/mussel treatment.csv")

############################################################################################################################
## create graph
############################################################################################################################

p1<-d_1 %>%
  ggplot(aes(x=mussel_length,y=mussel_weight))+
  geom_point()+
  geom_smooth(method="lm",formula=y ~ x+I(x^2),se=T)+
  theme_bw()+theme(legend.position="right")+
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x="mussel length (mm)", y=expression(mussel~weight~(ug)))
p1

############################################################################################################################
## save graph
############################################################################################################################
?ggsave

#How to save graphs into a specific folder in the Project
ggsave("figures/mussel correlation.png", p1, width = 10, height=5, units="in")

############################################################################################################################
## what is merge? what is join?
############################################################################################################################
?merge #baser. Automatically sorts df based on column you're using to merge
?left_join #dplyr - faster and doesn't auto sort 
############################################################################################################################
## inner join merge
############################################################################################################################

IJ <- merge(d_1, d_2) #Default is an inner join
IJ <- merge(x = d_1, y = d_2, by = c("tank_id", "tag")) # the same
IJ <- inner_join(d_1, d_2)

IJ
############################################################################################################################
## outer join merge
############################################################################################################################

OJ <- merge(d_1, d_2, all = T)

OJ
############################################################################################################################
## left join merge
############################################################################################################################
# matching all the rows in the first data frame with the corresponding values on the second
# should end up with # observations for first df
LJ <- merge(d_1, d_2, all.x = T)

LJ
############################################################################################################################
## right join merge
############################################################################################################################
# matching all the rows in the second data frame with the corresponding values on the first
# should end up with # observations for second df

RJ <- merge(d_1, d_2, all.y = T)

RJ
############################################################################################################################
## merge on merge
############################################################################################################################

JJ <- merge(d_3, merge(d_1, d_2, all = T))
