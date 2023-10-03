#Load in library ----

library(nycflights13)
library(tidyverse)
library(palmerpenguins)
library(ggthemes)


#Code starts here ----

view(palmerpenguins::penguins_raw)
glimpse(palmerpenguins::penguins)

summary(penguins)


#First two frames, defines y and x axis, miss the frame on top
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
)

#Geom introduced, third layer placed with the relations on length and body mass

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

#Final step is to add proper legends to the graph

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Bodymass and flipper length",
    subtitle = "Dimensions for Ade, Chi and Gen Penguins",
    x = "Flippa length", y = "Body mass",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()

palmerpenguins::penguins |> 
  ggplot(aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point() +
  geom_smooth()

#Usage of bar pointer