#Piping shortcut, ctrl + shift + m %>% 

#Joining fxns
require(dplyr)

band_members
band_instruments

#Look at ?inner to look at the different ways to join

inner_join(band_members, band_instruments)

left_join(band_members, band_instruments) #Had mick join and paul in members, so will add instruments to them
right_join(band_members, band_instruments) #same but opposite direction

full_join(band_members, band_instruments) #Fully combines data

#?inner_join gives lots of options like by


#What happens if the columns have different names
band_instruments2
left_join(band_members, band_instruments2,
          by = c("name" = "artist"))#Need a by because there is no similar colname

