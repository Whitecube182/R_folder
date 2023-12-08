#This project is written in one continues file

#1. Load lib ----

library(here)
library(tidyverse)
library(readxl)
library(psych)
library(janitor)
library(readxl)
library(ggplot2)
library(reshape2)

#If any of the packages are missing you can run the following (just remove the comment marker:
# To remove the comments on multiply lines, use CTRL+SHIFT+C
# install.packages(
#   c("arrow", "babynames", "curl", "duckdb", "gapminder",
#     "ggrepel", "ggridges", "ggplot", "ggthemes", "here", "hexbin", "janitor", "Lahman",
#     "readxl", "leaflet", "maps", "nycflights13", "openxlsx", "palmerpenguins", "psych",
#     "repurrrsive","reshape2", "tidymodels", "tidyvinstallerse", "writexl")
# )


#2. Data set 1 ----

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

#3. Data set 2 ----

#Names for the countries & import
countries <- c("India", "Egypt, Arab Rep.", "China", "Brazil")
gdp <- read_excel("datasets/gdp.xls", skip = 3)
population <- read_excel("datasets/population.xls", skip = 3)

#Cleaning names
gdp <- clean_names(gdp)
population <- clean_names(population)

#Filter data gdp
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

#Filter data population
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

#Move the dates to the first column, sub away x from dates, change Egypt to proper name
population_clean <- population_clean %>%
  rownames_to_column(var="Date")
population_clean$Date <- sub("^x", "", population_clean$Date)
names(population_clean)[4] <- "Egypt"

#Here we clean up the gdp_clean, correct name and moving in rows to column 1:

gdp_clean <- gdp_clean %>%
  rownames_to_column(var="Date")
gdp_clean$Date <- sub("^x", "", gdp_clean$Date)
names(gdp_clean)[4] <- "Egypt"

#Change population_clean to to numeric
population_clean$Brazil <- as.numeric(population_clean$Brazil)
population_clean$China <- as.numeric(population_clean$China)
population_clean$Egypt <- as.numeric(population_clean$Egypt)
population_clean$India <- as.numeric(population_clean$India)
population_clean$Date <- as.numeric(population_clean$Date)

#Gdp_clean as.numeric
gdp_clean$Brazil <- as.numeric(gdp_clean$Brazil)
gdp_clean$China <- as.numeric(gdp_clean$China)
gdp_clean$Egypt <- as.numeric(gdp_clean$Egypt)
gdp_clean$India <- as.numeric(gdp_clean$India)
gdp_clean$Date <- as.numeric(gdp_clean$Date)

#3.1 Join gdp and Population

population_clean_long <- pivot_longer(population_clean, cols = -Date, names_to = "Country", values_to = "Population")
population_clean_long$Date <- as.numeric(population_clean_long$Date)

gdp_clean_long <- pivot_longer(gdp_clean, cols = -Date, names_to ="Country", values_to = "GDP" )
gdp_clean_long$Date <- as.numeric(gdp_clean_long$Date)

gdp_pop <- left_join(population_clean_long, gdp_clean_long, by = c("Date", "Country"))
names(gdp_pop)[3] <- "Population"
names(gdp_pop)[4] <- "GDP"

#3.2 




#4. Visualzation data set 1 ----

#4.1 Indicies development graph (normalized) -----
#Visualizing the countries development(normalized)
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
    title = "Geometric returns 20 years",
    x = "Date",
    y = "Return",
    color = "Countries"
  ) +
  theme_minimal()

#4.2 Population graph ----

pop_clean_long <- population_clean %>%
  pivot_longer(cols = -Date, names_to = "Country", values_to = "Population") %>%
  mutate(Population = as.numeric(Population))

ggplot(pop_clean_long, aes(x = Date, y = Population, color = Country, group = Country)) +
  geom_line(size = 1) +
  labs(title = "Population Over Years",
       x = "Year",
       y = "Population",
       color = "Country") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, max(pop_clean_long$Population), by = 250000000), labels = scales::comma)

#4.2.1 Population facet graph

ggplot(population_clean_long, aes(x = Date, y = Population, fill = Country)) +
  geom_area() +
  labs(title = "Population Over Years",
       x = "Year",
       y = "Population",
       fill = "Country") +
  theme_minimal() +
  facet_grid(.~ Country, scales = "free_y")

#4.3.1 GDP facet grid ----

ggplot(gdp_clean_long, aes(x = Date, y = GDP, fill = Country)) +
  geom_area() +
  labs(title = "GDP Over Years",
       x = "Year",
       y = "GDP",
       fill = "Country") +
  theme_minimal() +
  facet_grid(. ~ Country, scales = "free_y")

#4.3.2 GDP Graph ----
#Make a graph of the gdp:

gdp_clean_long <- gdp_clean_long %>%
  mutate(Date = as.numeric(Date))

gdp_clean_long <- gdp_clean %>%
  pivot_longer(cols = -Date, names_to = "Country", values_to = "Population")

# Plot the development of each country

ggplot(gdp_clean_long, aes(x = Date, y = Population, color = Country, group = Country)) +
  geom_line(size = 1) +
  labs(title = "GDP Development Over Time",
       x = "Year",
       y = "GDP",
       color = "Country") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) 


ggplot(gdp_pop, aes(x = Date)) +
  geom_line(aes(y = Population, color = Country, group = Country)) +
  geom_bar(aes(y = GDP, fill = Country), stat = "identity", alpha = 0.5) +
  labs(title = "Population and GDP Over Years",
       x = "Year",
       y = "Values",
       color = "Country",
       fill = "Country") +
  theme_minimal() +
  facet_wrap(~ Country, scales = "free_y")


#4.3.3 GDP_POP represented in two graphs ----

ggplot(gdp_pop, aes(x = Date)) +
  geom_bar(aes(y = GDP, fill = Country), stat = "identity", alpha = 0.5) +
  labs(title = "GDP Over Years",
       x = "Year",
       y = "Values",
       color = "Country",
       fill = "Country") +
  theme_minimal() +
  facet_wrap(~ Country, scales = "free_y")

ggplot(gdp_pop, aes(x = Date)) +
  geom_bar(aes(y = Population, fill = Country), stat = "identity", alpha = 0.5) +
  labs(title = "Population Over Years",
       x = "Year",
       y = "Values",
       color = "Country",
       fill = "Country") +
  theme_minimal() +
  facet_wrap(~ Country, scales = "free_y")


#5. Final data representation ---- 

gdp_pop <- gdp_pop %>%
  mutate(GDP_per_Capita = GDP / Population)

ggplot(gdp_pop, aes(x = Date, y = GDP_per_Capita, color = Country)) +
  geom_line() +
  labs(title = "GDP per Capita Over Time",
       x = "Date",
       y = "GDP per Capita",
       color = "Country") +
  theme_minimal()

gdp_pop$Date <- as.factor(gdp_pop$Date)
gdp_pop$Country <- as.factor(gdp_pop$Country)

# New data frame for gdp per capita and the average return combined
average_gdp_per_capita <- gdp_pop %>%
  group_by(Country) %>%
  summarise(Average_GDP_Per_Capita = mean(GDP_per_Capita, na.rm = TRUE))

transposed_final_desc_data_log <- t(final_desc_data_log)
transposed_final_desc_data_log <- as.data.frame(t(final_desc_data_log))

mean_values <- transposed_final_desc_data_log %>%
  select(mean)

mean_values <- rownames_to_column(mean_values, "Country")
mean_values <- mean_values[-1, ]
mean_values$Country <- gsub("_Log", "", mean_values$Country, ignore.case = TRUE)
mean_values$Country <- gsub("EgyptN", "Egypt", mean_values$Country, ignore.case = TRUE)
merged_mean_gdp <- merge(mean_values, average_gdp_per_capita, by = "Country", all.x = TRUE)

ggplot(merged_mean_gdp, aes(x = Country)) +
  geom_bar(aes(y = Average_GDP_Per_Capita), stat = "identity", fill = "blue") +
  geom_text(aes(y = Average_GDP_Per_Capita, label = paste0(round(mean * 100, 2), "%")), vjust = -0.5) +
  labs(title = "Average GDP per Capita with Mean Values",
       y = "Average GDP per Capita",
       fill = "Legend") +
  theme_minimal()