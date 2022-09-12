library(readr)
library(ggplot2)
library(dplyr)
library(sf)
library(corrplot)

setwd("C:/Users/sophi/Documents/UNH_Docs/ModelingClass")

dat <- read_csv("data/JuvUVCSites_with_ReefTypes_16Jun2016.csv")
names(dat)

nrow(dat)
length(unique(dat$site))
range(dat$pres.topa)

ggplot(dat) + 
  aes(x = secchi, y = pres.topa) + 
  geom_point() + 
  stat_smooth()

ggplot(dat) + 
  aes(x = logged, y = pres.topa) + 
  geom_boxplot() +
  theme_set(theme_classic())


bendat <- read_csv("data/BenthicCoverSurveys.csv")
nrow(bendat)
head(bendat)

range(bendat$cover)

ggplot(bendat) + 
  aes(x = category,y = cover) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90))



#sf allows you to read spatial data
logponds <- st_read("data/Kia_Logging_Ponds/Kia_Logging_Ponds.shp")

names(logponds)

st_crs(logponds)#This checks Coordinate Reference System

land <- st_read("data/LandPoly/LandPoly.shp")
plot(land[1])

#Make a variable for cover of branching corals. Filter ACB or CB
CB_dat <- filter(bendat, code %in% c("ACB", "CB"))
#summarize over two benthic categories
CB_dat <- group_by(CB_dat, site)
CB_dat <- summarize(CB_dat, CB_cover = sum(cover),
                    n_pts = mean(n_pts))

soft_dat <- bendat %>%
  filter(code %in% c("S", "SI")) %>% #Sand or Silt
  group_by(site) %>% 
  summarize(soft_cover = sum(cover),
            n_pts = mean(n_pts))

#Now join fish and benthic dataframes
nrow(dat)
dat2 <- left_join(dat, CB_dat, by = "site")
nrow(dat2)


dat2 <- left_join(dat2, soft_dat, by = c("site", "n_pts"))
nrow(dat2)

ggplot(dat2) + 
  aes(x = logged, y = CB_cover) + 
  geom_boxplot()

ggplot(dat2) + 
  aes(x = CB_cover, y = pres.topa) + 
  geom_point()

ggplot(dat2) + 
  aes(x = CB_cover, y = soft_cover) + 
  geom_point()


#Now we explore correlations in data
icol <- sapply(dat2, is.numeric)
pairs(dat2[,icol])
round(cor(dat2[,icol]),2)

#You can also summarize as a corrplot:
icol <- sapply(dat2, is.numeric)
corrplot(cor(dat2[,icol]))

#Now creating spatial points files - convert spatial data to survey dataframe
kia_crs <- st_crs(logponds)
sdat2 <- st_as_sf(dat2, coords = c("coordx", "coordy"),
                  crs = kia_crs)
plot(sdat2["pres.topa"])
#st_as_sf is the function that converts different data types to simple features. Try ?st_as_sf to see what else it can convert. 

#Check that all data has same CRS
st_crs(land) == kia_crs
#To correct CRS to that of logponds:
land <- st_transform(land, kia_crs)
st_crs(land) == kia_crs

#Make a matrix of distances from every sample site to every logpong
distmat <- st_distance(sdat2, logponds)
dim(distmat)
#For each site find the min distance
apply(distmat, 1, min)[1:5]

sdat2$dist_to_logging <- apply(distmat, 1, min)/1000

ggplot(sdat2) + 
  aes(x = dist_to_logging, y = pres.topa) + 
  geom_point()


#Save clean dataset MAKE THIS WORK BEFORE MOVING ON
save(sdat2, dat2, land, logponds, kia_crs, 
     file = "outputs/2021-09-20_cleaned-data.rda")

#Note; how to calcute distances: https://www.seascapemodels.org/rstats/2020/02/08/calculating-distances-in-R.html








