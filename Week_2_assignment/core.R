#Load in library ----

library(nycflights13)
library(tidyverse)


#Code starts here ----

view(nycflights13::flights)
summary(nycflights13::flights)

filter(nycflights13::flights, arr_time >= 1800)


#We are looking for 6pm (1800) until 1159pm (23:59)
filter(nycflights13::flights, between(arr_time, 1800, 2359))

# select(), contains(), 

select(nycflights13::flights, contains("dep_delay"))

# Find all flights that made up at least 10 minutes of delay
# at departure during the flight




#By default, missing values are placed at the end of the 
#the resulting tibble. How could you use arrange() to sort
#all the missing values to be at the start?

arrange(nycflights13::flights, desc(is.na(dep_time)))




flights_times <- mutate(nycflights13::flights,
  dep_time_mins = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
  sched_dep_time_mins = (sched_dep_time %/% 100 * 60 +
  sched_dep_time %% 100) %% 1440
)