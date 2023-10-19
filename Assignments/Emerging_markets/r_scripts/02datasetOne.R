#Load lib ----

library(here)
library(tidyverse)
library(readxl)
library(psych)

#Data transformation for the indices ----
#Load data from excel file and place it in a object

index_emerging_markets <- read_excel("datasets/FTSE 20.09.23 - EOH.xlsx", skip = 6)


#Change the name of the first column to date

index_emerging_markets <- index_emerging_markets |>
  rename(
    Date = Name
  )

#We want to normalize the values with the base 100 and add these to the table

normalized <- index_emerging_markets |>
  mutate(
    FTSEWorldN = FTSEWorld / (146.2 / 100),
    IndiaN = India / (332.5 / 100),
    EgyptN = Egypt / (59.04 / 100),
    ChinaN = China / (672.28 / 100),
    BrazilN = Brazil / (150.65 / 100),
  )

#Select only the normalized data

normalized_trimmed <- normalized |>
  select(Date, FTSEWorldN:BrazilN)

#Create a new section of columns for logarithmic (log10) returns

normalized_trimmed <- normalized_trimmed |>
  mutate(
    FTSEWorld_Log = c(NA, log(FTSEWorldN[-1] / FTSEWorldN[-nrow(normalized)])),
    India_Log = c(NA, log(IndiaN[-1] / IndiaN[-nrow(normalized)])),
    EgyptN_Log = c(NA, log(EgyptN[-1] / EgyptN[-nrow(normalized)])),
    China_Log = c(NA, log(ChinaN[-1] / ChinaN[-nrow(normalized)])),
    Brazil_Log = c(NA, log(BrazilN[-1] / BrazilN[-nrow(normalized)])),
  )

#Multiply each of the log returns by 100 to show them in %
normalized_trimmed <- normalized_trimmed |>
  mutate(
    FTSE_per = FTSEWorld_Log * 100, 
    India_Per = India_Log * 100,
    Egpyt_per = EgyptN_Log * 100, 
    China_per = China_Log * 100, 
    Brazil_per = Brazil_Log * 100,
  )

#Put log to another df

log_return <- select(normalized_trimmed,
    -"FTSEWorldN":-"BrazilN"
  )

log_return <- log_return[-1, ]

#Export descriptive_data from the normalized frame to a new object

desc_data <- describe(normalized_trimmed) |>
  t()

#Make it back to a df from a matrix
desc_data <- as.data.frame(desc_data)

#Remove rows with information we do not need
desc_data <- desc_data[-c(1, 5:9), ]


#Remove the column date as dates isn't a thing in the descriptive statistics
desc_data <- select(desc_data,
                    -"FTSE_per":-"Brazil_per"
                    )

final_desc_data_log <- select(desc_data,
                              FTSEWorld_Log:Taiwan_Log
                              )

#write the data to a csv file in our project folder

write.csv(transp_desc_data, "exported_data/desc_data.csv")


