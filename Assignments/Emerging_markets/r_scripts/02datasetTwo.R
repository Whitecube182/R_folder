

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


