## 2021 09 16

download.file("https://raw.githubusercontent.com/swcarpentry/r-novice-gapminder/gh-pages/_episodes_rmd/data/gapminder_data.csv", destfile = "09.16_PlottinginR/data/gapminder_data.csv")
gapminder <- read.csv("09.16_PlottinginR/data/gapminder_data.csv", stringsAsFactors = TRUE)

library(ggplot2)

#Look at data using head(), dim(), summary(), view()
  #You can specify the # of rows you want to see in head(data, #)

ggplot(data = gapminder, aes(x = gdpPercap, y  = lifeExp)) +
  geom_point()
  #aes stands for aesthetic. Sets up how data is displayed
  #geom means geometry
  #Could also do p <- ggplot (...) and then run or add onto p



ggplot(data = gapminder, aes(x = year, y = lifeExp, color = continent, by = country)) +
  geom_point(color = "black") +
  geom_line()
#aes sets up default colors, while specifying things within the geom commands will change it locally within command
#ggplot is layered, so the last command will be layered on top
#Use ggsave to save. Look into ?ggsave



ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point() +
  scale_x_log10() + #make log scale
  geom_smooth(method = "lm", color = "green") #Add curve and error lines, lm makes it a linear model. If you forget to specify color, ggplot will assign default from aes (in this case by continent)

#Two second linear model
summary(lm(gapminder$lifeExp ~ gapminder$gdpPercap))







