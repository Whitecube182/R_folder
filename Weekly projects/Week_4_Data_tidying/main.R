#LIbrarys ----

library(tidyverse)

#Main body ----

Turb_rate <- table1 |>
  mutate(rate = cases / population * 10000)

Turb_rate |>
  group_by(year) |>
  summarize(total_cases = sum(cases))

view(table1)

ggplot(table1, aes(x = year, y = cases)) +
  geom_line(aes(group = country), color = "grey50") +
  geom_point(aes(color = country, shape = country)) +
  scale_x_continuous(breaks = c(1999, 2000))

#The three lines above are doing the following:
#1. checks the rate per 10 000
#2. Groups by year then sum the amount of cases per year in two new rows
#Last one plots the case rate with a line and end points being symbols

Turb_rate |>
  group_by(country) |>
  summarize(total_cases = sum(cases))


Turb_rate <- Turb_rate |>
  group_by(country) |>
  summarize(total_cases_per = sum(cases) / sum(population) * 10000)

#Above is a bad use of objects, the old Turb_rate will not be there anymore
#This summarize the amount of cases for each country

#New Example - Billboard ----

view(billboard)

new_billboard <- billboard |> 
  pivot_longer(
    cols = starts_with("wk"), 
    names_to = "week", 
    values_to = "rank"
  )

#all columns starting with wk gets a new column, 
#It then names them Week 1, 2, 3 etc
#All the values are moved to rank 

test <- new_billboard |>
  arrange(rank) |>
  filter(week == "wk1")
