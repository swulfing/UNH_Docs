# Load packages:
library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(mapview)
library(rnoaa)

# Unzip spatial datasets
unzip("./data.zip",overwrite=TRUE)

# Read in spatial data
local_lakes <- st_read("./data/NHDWaterbodyDataset_NHSeacoast.shp")

# Manipulate spatial data
swains_lake <- local_lakes %>%
  filter(GNIS_Nm == "Swains Lake")

# Find nearest weather station
stations <- read_sf("./data/NH_GHCND_stations.shp") 

# Estimate distance between each weather station and Swains Lake
st_distance(stations,swains_lake)

# Transform data to CONUS Albers Equal Area Conic projection:
swains_lake_proj <- st_transform(swains_lake,5070)
stations_proj <- st_transform(stations, 5070)

# Estimate distance between each weather station and Swains Lake
stations_proj2 <- stations_proj %>%
  mutate(dist = st_distance(.,swains_lake_proj,by_element = TRUE))

# Spatial join: Find the closest weather station to Swains Lake
closest_station <- st_join(swains_lake_proj,stations_proj,join=st_nearest_feature)

# Download precipitation data
prcp_data <- rnoaa::ghcnd_search(closest_station$id, var = "PRCP")

# Create a site map
ggplot() + 
  geom_sf(data=swains_lake) + 
  coord_sf()