require(tidyr)
#Long > wide format or vice versa
gap_wide <- read.csv("gapminder.csv", header = T)

# Make it long!
gap_long <- gap_wide %>%
  pivot_longer(cols = c(starts_with("gdpPercap"), starts_with("lifeExp"), starts_with("pop")),
               names_to = "measurement",
               values_to = "amount")

gap_long <- gap_long %>%
  separate(col = measurement,
           into = c("measurement", "year"),
           sep = "_")

# Going back to wide
gap_wide2 <- gap_long %>%
  pivot_wider(names_from = measurement,
              values_from = amount
              )


gap_wide3 <- gap_long %>%
  pivot_wider(names_from = year,
              values_from = amount
  )

gap_wide4 <- gap_long %>%
  pivot_wider(names_from = c(measurement, year),
              values_from = amount
  ) #Now combines meas and year into one column. separates by _

# Cheat Sheet
# https://github.com/rstudio/cheatsheets/blob/main/tidyr.pdf