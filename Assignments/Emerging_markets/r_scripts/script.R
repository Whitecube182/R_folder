#This project will be written in one long file, having multiply did not really
#help. Only made things harder to keep track of.  

#1. Load lib ----

library(here)
library(tidyverse)
library(readxl)
library(psych)
library(janitor)
library(readxl)
library(ggplot2)

#If any of the packages are missing you can run the following (just remove the comment marker:

# install.packages(
#   c("arrow", "babynames", "curl", "duckdb", "gapminder",
#     "ggrepel", "ggridges", "ggplot", "ggthemes", "here", "hexbin", "janitor", "Lahman",
#     "readxl", "leaflet", "maps", "nycflights13", "openxlsx", "palmerpenguins", "psych",
#     "repurrrsive", "tidymodels", "tidyvinstallerse", "writexl")
# )


#2. df data set 1 ----

#Data transformation for the indices
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
                              FTSEWorld_Log:Brazil_Log
)

#write the data to a csv file in our project folder

write.csv(final_desc_data_log, "exported_data/desc_data_log.csv")



#3. df data set 2 ----


#Names for the countries
countries <- c("India", "Egypt, Arab Rep.", "China", "Brazil")


#Import the data set from the file population

gdp <- read_excel("datasets/gdp.xls", skip = 3)

#Import data set from the file gdp

population <- read_excel("datasets/population.xls", skip = 3)

#The two files above are downloaded from The World Bank and are accessible from
# https://data.worldbank.org


#To do list
#clear out the two sets from irrelevant data
#Join the two data sets into one df
#Transform and derive data based on these two metrics

gdp <- clean_names(gdp)
population <- clean_names(population)


gdp_clean <- select(gdp,
                    -"x1960":-"x2002",
                    -"indicator_code",
                    -"indicator_name",
                    -"country_code"
) |>
  filter(
    country_name %in% countries
  ) |>
  t() |>
  row_to_names(row_number = 1) |>
  as.data.frame()

population_clean <- select(population,
                           -"x1960":-"x2002",
                           -"indicator_code",
                           -"indicator_name",
                           -"country_code"
) |> 
  filter(
    country_name %in% countries
  ) |>
  t() |>
  row_to_names(row_number = 1) |>
  as.data.frame()

#3. Visualzation data set 1 ----

#Cleared the whole thing as it wasn't working. Need to put in some
#hours and get a more robust frame before my meeting next week!


ggplot(
  data = normalized_trimmed,
  mapping = aes(x = Date, y = FTSEWorldN )
) +
  geom_line( mapping = aes())


ggplot(
  data = normalized_trimmed,
  mapping = aes(x = Date, y = IndiaN )
) +
  geom_line( mapping = aes())

ggplot(
  data = normalized_trimmed,
  mapping = aes(x = Date, y = EgyptN )
) +
  geom_line( mapping = aes())

ggplot(
  data = normalized_trimmed,
  mapping = aes(x = Date, y = ChinaN )
) +
  geom_line( mapping = aes())

ggplot(
  data = normalized_trimmed,
  mapping = aes(x = Date, y = BrazilN )
) +
  geom_line( mapping = aes())

ggplot(
  data = normalized_trimmed,
  mapping = aes(x = Date)
) +
  geom_line(aes(y = FTSEWorldN, color = "FTSEWorldN")) +
  geom_line(aes(y = IndiaN, color = "IndiaN")) +
  geom_line(aes(y = EgyptN, color = "EgyptN")) +
  geom_line(aes(y = ChinaN, color = "ChinaN")) +
  geom_line(aes(y = BrazilN, color = "BrazilN")) +
  scale_color_manual(values = c("FTSEWorldN" = "red", "IndiaN" = "green", "EgyptN" = "blue", "ChinaN" = "orange", "BrazilN" = "purple")) +
  labs(
    title = "Geometric eturns 20 years",
    x = "Date",
    y = "Return",
    color = "Countries"
  ) +
  theme_minimal()
