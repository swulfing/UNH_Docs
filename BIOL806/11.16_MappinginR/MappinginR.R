#https://www.r-graph-gallery.com/ to look at different projections or packages for mapping
#https://datacarpentry.org/r-raster-vector-geospatial/

library(gtrendsR)
library(tidyverse)
library(usmap)

thanksgiving <- gtrends('thanksgiving', geo = 'US', time = 'now 1-d') #look into gtrends to see different time inputs

thanksgivingStates <- thanksgiving$interest_by_region
thanksgivingStates$fips <- fips(thanksgivingStates$location)

View(thanksgiving)

orange <- "#C9592E"

my_points <- data.frame(lon = -70.9269444, lat = 43.1338889)
my_points <- usmap_transform(my_points)

plot_usmap(data = thanksgivingStates, values = "hits", color = orange, labels = FALSE) +
  scale_fill_continuous(low = "white", high = orange, name = "Popularity", label = scales::comma) +
  theme(legend.position = "right") + 
  theme(panel.background = element_rect(colour = "black")) +
  labs(title = "Popularity of Thanksgiving Google Search by State", caption = "@eastonrwhite") + 
  geom_point(data = my_points, aes(x = lon.1, y = lat.1))