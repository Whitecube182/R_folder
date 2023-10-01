#Loading librarys ----

library(nycflights13)
library(tidyverse)


#Code starts here ----

#?flights shows the documentation for the data set

?flights

#view(flights) opens a separated window with the data

view(flights) 
summarize(flights)

#glimpse shows the amount of columns and Rows for the data set

glimpse(nycflights13::flights)

#In glimpse we can see <int>, <dbl>, <dttm>, these are all
#information about what type of data the column is in

nycflights13::flights |>
  filter(dest == "IAH") |>
  group_by(year, month, day) |>
  summarize(
    arr_delay = mean(arr_delay, na.rm = TRUE)
  )

#The line of code above uses |> (pipe), this can be translated to
#then. So the line above is first the data --> filter by destination
#group by year, month, day --> than summarize the arr_delay data
#na.rm = TRUE does one thing, it excludes missing values

flights |>
  filter(dep_delay > 120)

#Here we filtered departure delays that were greater than 120 minutes

flights |>
  filter(day == 1 & month == 1 & dep_delay >= 120)

#The code above filters with Jan 1 departure delay equal or greater than 120

flights |>
  filter(month %in% c(1, 2))

#Here we are using two new commands, c() and %in%. 
#c() is simply explained as combine
#%in%, I don't understand it fully yet but here is a link to an exp:
# https://rdrr.io/r/base/match.html
# The text (ch. 4) says we'll get back to this operator

jan1 <- flights |>
  filter(month == 1 & day == 1)

#Here we assigned jan1 to january 1, filtered

view(jan1)

#View jan1 will show this variable in a new table

flights |>
  filter(month = 1)

#This code is incorrect due to the use of incorrect use of = instead of ==

test <- flights |>
  arrange(year, month, day, dep_time)
view(test)

#The code above instruct to arrange the data after the order specified
#So it orders after years, month, day, dep_time

flights |>
  arrange(desc(dep_delay))

#Desc is a shorthand for descending order, therefore the line above tells
#to arrange dep_delay in descending order

flights |>
  distinct()

#distinct() removes any duplicated rows

origin_dest <- flights |>
  distinct(origin, dest)

#Find all unique origin and destination pairs

#To keep all the columns we can do the following:

keepAllDestOrigin <- flights |>
  distinct(origin, dest, .keep_all = TRUE)

#I saved this to a variable in the global environment to view it

flights |>
  count(origin, dest, sort = TRUE) |>
  view()

#The line above counts each instance of flights and presents the
#amount of occurance in a new column called n

#Exercises ----

flights |> 
  filter(arr_delay >= 120)

#Filters any flight delayed by more than two hours

dest_IAH_HOU <- flights |>
  filter(dest == c("IAH","HOU")) +
  view(dest_IAH_HOU)

#Filters any flight flying to Houstons' two airports

carrier_list <- c("UA", "AA", "DL")

carrier_UAAADL <- flights |>
  filter(carrier %in% carrier_list)

view(carrier_UAAADL)

#The code above makes a combined list of the three variables
#The variables gets saved in carrier_list
#The second command saves filtered flights to carrier_UAAADL

m_jul_sep <- flights |>
  filter(month %in% c(7, 8, 9 ))

view(m_jul_sep)

#The code above looks at all flights in July Aug and Sept

flights |>
  filter(dep_delay <= 0 & arr_delay >= 120)

#This is no departure delay and more than 2 hours arrival delay

#Final: Were delayed by at least an hour but made up over 30 min in flight

flights |>
  filter(dep_delay >= 60 & dep_delay - arr_delay > 30)

#Sort flights that departured later than 60 minutes after schedule and made up
#more than 30 minutes in flight

flights |>
  arrange(desc(dep_delay))

#Arrange the tibble by dep_delay in descending order

flights |>
  arrange(desc( distance / air_time))

#Sort the fastest flights in order, highest number is distance/time = speed

flights |>
  filter(year == 2013) |>
  distinct(month, day)

#Check if there was a flight each day of the year, it produces 365 rows
#meaning that there was a flight each day of the year (2013 is not a leap year)

flights |>
  arrange(distance)

flights |>
  arrange(desc(distance))

#Arrange for longest and shortest distance, both gets answered by 
#the first line in the tibble presented in the console

#Random tests ----


#Testing to write csv of summary variable

sumDataFlights <- summary(flights)
write.csv(sumDataFlights, "test.csv", row.names=FALSE)

#It works and the file is exported to the local folder for the project

read.csv("~test.csv") |>
  view("~test.csv")

#This reads the file first then opens it in a window


