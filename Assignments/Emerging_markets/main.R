#Load lib ----

#install.packages("psych") if needed
#install.packages("here") if needed

library(here)
library(tidyverse)
library(readxl)
library(psych)

#Data transformation for the indices ----





#Load data from excel file and place it in a object

index_emerging_markets <- read_excel("FTSE 20.09.23 - EOH.xlsx", skip = 6)

#Quick look at the tibble
glimpse(index_emerging_markets)

#Change the name of the first column to date

index_emerging_markets <- index_emerging_markets |>
  rename(
    Date = Name
  )

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
  select(Date, FTSEWorldN:TaiwanN)

#Create a new section of columns for logarithmic (log10) returns

normalized_trimmed <- normalized_trimmed |>
  mutate(
    FTSEWorld_Log = c(NA, log(FTSEWorldN[-1] / FTSEWorldN[-nrow(normalized)])),
    India_Log = c(NA, log(IndiaN[-1] / IndiaN[-nrow(normalized)])),
    EgyptN_Log = c(NA, log(EgyptN[-1] / EgyptN[-nrow(normalized)])),
    China_Log = c(NA, log(ChinaN[-1] / ChinaN[-nrow(normalized)])),
    Brazil_Log = c(NA, log(BrazilN[-1] / BrazilN[-nrow(normalized)])),
    Taiwan_Log = c(NA, log(TaiwanN[-1] / TaiwanN[-nrow(normalized)])),
  )

#Multiply each of the log returns by 100 to show them in %
normalized_trimmed <- normalized_trimmed |>
  mutate(
    FTSE_per = FTSEWorld_Log * 100, 
    India_Per = India_Log * 100,
    Egpyt_per = EgyptN_Log * 100, 
    China_per = China_Log * 100, 
    Brazil_per = Brazil_Log * 100,
    Taiwan_per  = Taiwan_Log * 100,
  )

#Export descriptive_data from the normalized frame to a new object

desc_data <- describe(normalized_trimmed)

#Transpose the descriptive data to 

transp_desc_data <- t(desc_data)

#write the data to a csv file in our project folder

write.csv(transp_desc_data, "exported_data/desc_data.csv")


