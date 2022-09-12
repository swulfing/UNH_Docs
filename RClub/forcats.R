library(dplyr)
library(ggplot2)
library(forcats)

starwars

#orgaize order of bars by frequency
ggplot(starwars, aes(x = fct_infreq(hair_color))) +
  geom_bar() +
  coord_flip()

#Lumping
starwars %>%
  count(skin_color, sort = TRUE)

starwars %>% #Sort AND organize by counts
  mutate(skin_color = fct_lump(skin_color), n = 5) %>%
  count(skin_color, sort = TRUE)

# Relevel by other variable
avg_mass_eye_color <- starwars %>%
  mutate(eye_color = fct_lump(eye_color, n = 6)) %>%
  group_by(eye_color)%>%
  summarize(mean_mass = mean(mass, na.rm = TRUE))

avg_mass_eye_color

avg_mass_eye_color %>%
  mutate(eye_color = fct_reorder(eye_color, mean_mass)) %>%
  ggplot(aes(x = eye_color, y = mean_mass)) +
  geom_col()


#Manual relevel
starwars$sex <- as.factor(starwars$sex)
  ggplot(starwars, aes(x = sex)) +
  geom_bar() +
  coord_flip()

levels(starwars$sex)
ggplot(starwars, aes(x = fct_relevel(sex, c("none", "hermaphroditic", "female", "male")))) +
  geom_bar() +
  coord_flip()
