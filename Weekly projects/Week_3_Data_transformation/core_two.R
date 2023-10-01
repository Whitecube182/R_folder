#Session started 30-09-2023 (ddmmyyyy)
#Libs ----

library(tidyverse)
library(nycflights13)

#Code starts here ----
#Unlike common convention I will comment below each line of code instead of above

flights |>
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .before = 1
  )

#In this command we are creating two new columns
#Gain and speed, 
#The .before instruct to place them at position one, before the other
#columns

flights |>
  mutate(
    total_minutes = hour * 60 + minute,
    .before = 1,
  )

#This shows how many minutes after midnight the plane left

flights |>
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hour, 
    .keep = "used"
  )

#Only new thing here is ".keep = "used", 
#This line instructs the output to only involve the used columns

#This will cover select(), 

flights |>
  select(year:day)

#Selects everything between year and day column
#Year:day, same as marking cells in excel

flights |>
  select(year, day)

#specific selection

flights |>
  select(!year:day)

#! is a "not", selects everything except year, month and day

flights |>
  select(where(is.character))

#Only selects strings

#Select have several functions that can help
#starts_with()
#ends_with("xyz")
#contains("lalaa")
#num_range("x", 1:3)
#?select for more information

flights |>
  select(tail_num = tailnum) 

#Here we rename variables, left side 

flights |>
  rename(tail_num = tailnum)

#Renames the column
#Doing all manually can be very ineffective if the data is messy
#Check out janitor::clean_names() for effective handling

flights |>
  relocate(time_hour, air_time)

#This moves the two columns to the start of the tibble
#.before .after can be used here just like in mutate()

flights |>
  relocate(year:dep_time, .after = time_hour)

flights |>
  relocate(starts_with("arr"), .before = time_hour)

#Exercises 4.3.5

delays <- flights |>
  select(dep_time, sched_dep_time, dep_delay)

#Selects the three columns
#2. Just use the . commands on line 65:68
#3.

flights |>
  select(dep_time, dep_time)

#It removes the doubles and only presents one of them

variables <- c("year", "month", "day", "dep_delay", "arr_delay")
flights %>% 
  select(any_of(variables))

#Just selects all of them?

contains_time <- select(flights, contains("TIME"))

view(contains_time)

#The raltionship between departure, schedueled departure and delay:
# dep_time - scheduled dep time = delay

variables <- c("year", "month", "day", "dep_delay", "arr_delay")

view(variables)

#This objects only saves the actual names in the c() commande

test <- flights |>
  select(all_of(variables))

view(test)

flights |>
  select(air_time = airtime)


#4.4 The pipe, the strength is to combine multiply verbs

search_funk <- flights |>
  filter(dest == "IAH") |>
  mutate(speed = distance / air_time * 60) |>
  select(year:day, dep_time, carrier, flight, speed) |>
  arrange(desc(speed))

#Filter destination to IAH, adds the column speed, selects, arranges

library(tidyverse)

view(mtcars)

mtcars %>%
  group_by(cyl) |>
  summarize(n = n())

#two different kinds of pipes |> and %>%
#they behave a bit different in more complex operations

flights |>
  group_by(month) |>
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE)
  )

#group by month, summarize the average dep_delay and ignore the empty cells


