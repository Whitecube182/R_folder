#Requierments for this file ----

#Here I list all the expected packages that should be installed
#If any are missing there are commented areas with the install lines

library(tidyverse)
library(palmerpenguins)
library(ggthemes)

#Working space ----


ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) +
  geom_point() +
  geom_smooth(method = "lm")
