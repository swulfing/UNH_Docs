library(tidyverse)
library(dplyr)
library(readr)
library(tidyr)

#Tidyverse packages shar an underlying design philosophy, gtammar, and data structure. The core packages are dplyr, ggplot2, tidyr, readr, pufff, stingr

#Philosophy: recognizes that most R users aren't programmers. Tidyverse recognizes that there are standard components of ata science. 

#Import (readr), tidy, transform (dplyr), visualizing (ggplot), model (iterative), communicate (tidyverse does first four)

###Grammar and structure

#I don't have the data 
my_data_readr <- read_csv(url(urlfile))
#Read r will automatically report back data info as well as classifying column types
#base r is read.csv, and it will read back similar info

#loading tidyverse will load all of these
#likes data in few columns, lots of rows
data <- pivot_longer(data, cols = colnames(data[,2:316]), names_to = "newname") #Collapse columns into 1 colum counts every row, all but first col

#How to look at things within packages ?tidyr::pivot_longer

#Now transform using dplyr, cheat sheets exist online

newdata <- select(data, colname)#data and then columns that you want

#what if we want all maple. What if we want column names that start ACE

maples <- select(data, contains("ACE"))
maples <- select(data, starts_with("ACE"))

#piping
maples2 <- data %>% select(starts_with("ACE"))# %>%  shortcut ctrl+shift+m. Another way for same result. We will get to difference later

#Let's apply select and piping to lengthening process above
tidy_data2 <- data %>% pivot_longer(cols = colnames(data %>% select(-x))) #Everything except x

#Change column names
tidy_data2_cols_changed <- tidy_data2 %>% rename(plot = x, species = name, abund = value) #newname = oldname

#We can do all of this with just piping

tidy_data2 <- data %>% pivot_longer(cols = colnames(data %>% select(-x))) %>% rename(plot = x, species = name, abund = value) 


#How to select rows: use filter
somedata <- tidy_data2 %>% filter(species == "abibal") #Colname = unit
#only includes rows where abibal is present

#What if we were interested in plots where abibal was presetn

data_with_abibal <- tidy_data2 %>% filter(species == "abibal") %>% filter(abundance > 0)

#combine tables
soils_clean <- soils %>% select(-x1) %>% relocate(Site, starts_with("0"), starts_with("B"), starts_with("C"))

#now combine the soil and plant data by plot number
setdiff(soils_clean$site, veg_data$x) #see what data is in siol data that isn't in veg data. Tells you which data to put glue onto the other(left or right join)

merged <- left_join(rename(veg_data, Site = X), soils_clean, by = "Site") #can do a full join. Where there is missing data, it will put NA values

#summarize cases. What is the least abundant plant?
tidy_data2 %>% group_by(species) %>% summarize(avg = mean(abund)) #group by species and then calc average abundance











