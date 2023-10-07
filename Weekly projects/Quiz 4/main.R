#lib ----

library(tidyverse)

#Body ----

view(table2)
view(table3)
view(table1)

#Quick check for the quiz question

newTable2 <- table2 %>% pivot_wider(names_from = year, values_from = count)

newTable2 %>% pivot_wider(names_from = type, values_from = 1999)

table2 %>% pivot_wider(names_from = type, values_from = count) %>% pivot_wider(names_from=year, values_from=cases)

#Consider the following

mydata <- tribble(
  ~month, ~male, ~female,
  "month1", NA, 10,
  "month2", 20, 12
)

#Change months to number and 

tidy_data <- mydata %>%
  pivot_longer(cols = c(male, female), names_to = "gender", values_to = "count") %>%
  mutate(gender = ifelse(gender == "male", "Male", "Female"))

tidy_data <- tidy_data %>%
  mutate(month = as.numeric(gsub("month", "", month)))

#Here we use ac ouple of different functions
# c(combine)
# ifelse(logic statemnet, "if true", "if false")
# pivot_longer(cols, n)


view(table1)

rate_table <- table1 |>
  mutate(rate = cases / population * 10000)

#Makes a new object with rate of cases

table1 |>
  group_by(year) |>
  summarize(total_cases = sum(cases))

#Summarize the data of table1 by grouping the data by year, than summarize.
#The caluclation actualy only takes each group and count them, 

table1 |>
  summarize(total_cases = sum(cases))

#Skipping the group by we separate and sum the whole table of cases 

table1 |>
  group_by(country, year) |>
  summarize(total_pop = sum(population))

#Here we only get the original table as it is ordered in the same way I used group_by

