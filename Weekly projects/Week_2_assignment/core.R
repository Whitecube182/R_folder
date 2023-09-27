#Load in library ----

library(nycflights13)
library(tidyverse)
library(palmerpenguins)
library(ggthemes)


#Code starts here ----

view(palmerpenguins::penguins_raw)

glimpse(palmerpenguins::penguins)


#First two frames, defines y and x axel, miss the fram on top
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
)

#Geom introduced, third layer placed with the relations on length and bodymass

ggplot (
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point()

#Same as above but added colors to the different species,
#NB! Max 6 colors available?! I think

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) +
  geom_point()

#Same as above, added an extra layer with the smooth lines
#Unsure about the argument "lm" and what it actually does

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) +
  geom_point() +
  geom_smooth(method = "lm")



#Next step we remove the color from aes and move it to geom point
#This is to assure that we give it one long line instead of adding 
#One for each species

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species, shape = species)) +
  geom_smooth(method = "lm")

#Might have been mistaken before, might be 6 shapes instaed of colors

