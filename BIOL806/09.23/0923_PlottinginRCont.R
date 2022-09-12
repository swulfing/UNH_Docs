gapminder <- read.csv("09.16_PlottinginR/data/gapminder_data.csv", stringsAsFactors = TRUE)

library(ggplot2)
library(viridis)

ggplot(data = gapminder, aes(x = lifeExp , y = gdpPercap, color = continent)) +
  geom_point() +
  theme_minimal() +
  labs(color = "Contintent", x = "Life Expectancy", y = "GDP Per Capita") +
  scale_color_viridis_d()

#geom_label(x = 40, y = 6000, label = "ringadingding") I can't get this to not be weird

  #scale_color_brewer(palette = "PuBuGn") + Can use either color brewer in ggplot or load package viridis
  #theme(legend.position = "top",
  #      legend.direction = "vertical") #theme allows you to fine tune graphs to be preeeeety. See ?theme





#MARKDOWN IDEAS
#In markdown, you can put in mini notebooks of code within text with: 
#'r blah blah blah'

#As opposed to:
#'''{r} blah blah blah'''

#putting ".." into file directory will go back up one folder and then 
#eg. source(file = "..data/something/something")
