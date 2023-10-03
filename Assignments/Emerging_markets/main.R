#Load lib ----

#install.packages("psych") if need be

library(tidyverse)
library(readxl)
library(psych)

#Data transformation for the indices ----


#Load data from excel file and place it in a object

index_emerging_markets <- read_excel("FTSE 20.09.23 - EOH.xlsx", skip = 6)

#Quick look at the tibble
glimpse(index_emerging_markets)

#We want to normalize the values with the base 100 and add these to the table

normalized <- index_emerging_markets |>
  mutate(
    FTSEWorldN = FTSEWorld / (146.2 / 100),
    IndiaN = India / (332.5 / 100),
    EgyptN = Egypt / (59.04 / 100),
    ChinaN = China / (672.28 / 100),
    BrazilN = Brazil / (150.65 / 100),
    TaiwanN = Taiwan / (182.21 / 100),
  )

#Select only the normalized data

normalized_trimmed <- normalized |>
  select(Name, FTSEWorldN:TaiwanN)

#Export descriptive_data from the normalized frame to a new object

desc_data <- describe(normalized_trimmed)

#Transpose the descriptive data to 

transp_desc_data <- t(desc_data)

#write the data to a csv file in our project folder

write.csv(transp_desc_data, "exported_data/desc_data.csv")


