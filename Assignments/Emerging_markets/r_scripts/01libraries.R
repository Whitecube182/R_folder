#Load lib ----

library(here)
library(tidyverse)
library(readxl)
library(psych)
library(janitor)
library(readxl)

#If any of the packages are missing you can run the following (just remove the comment marker:

# install.packages(
#   c("arrow", "babynames", "curl", "duckdb", "gapminder", 
#     "ggrepel", "ggridges", "ggplot", "ggthemes", "here", "hexbin", "janitor", "Lahman", 
#     "readxl", "leaflet", "maps", "nycflights13", "openxlsx", "palmerpenguins", "psych",  
#     "repurrrsive", "tidymodels", "tidyverse", "writexl")
# )