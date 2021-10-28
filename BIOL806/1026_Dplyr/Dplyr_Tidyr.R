
gapminder <- read.csv("09.16_PlottinginR/data/gapminder_data.csv", stringsAsFactors = TRUE)

require(dplyr) #Downloads if you don't have it
require(tidyr)


### Dplyr ###

#Returns dataset with means and sd of countries with pops greater than 1,000,000
gapminder_means <- gapminder %>% 
  select(country, pop) %>%
  filter(pop > 1000000) %>%
  group_by(country) %>% #Doesn't quite do anything yet, just secretly stores as different data sets.
  summarize(mean_pop = mean(pop), sd_pop = sd(pop))

#adding a column
gapminder_means$new_col <- gapminder_means$mean_pop^2

### TidyR ###
#I don't have side dataset so this won't really make sense
gap_long <- gap_wide %>%
  pivot_longer(
    cols = c(starts_with("pop"), starts_with("lifeExp"), starts_with("gdpPerCap")),
    names_to = "obstype_year", values_to = "obs_values")

gap_long <- gap_long %>%
  separate(obstype_year, into = c("obs_type", "year"), sep = "_")

#How to make wide format
gap_normal <- gap_long %>%
  pivot_wide(names_from = obs_type, values_from = obs_values)