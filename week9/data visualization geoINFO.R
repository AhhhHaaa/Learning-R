
# 1.Exploring time-series data ----------------------------------------------
#prerequisite packages
library(tidyverse)
library(lubridate) 
library(nycflights13)

#add a new column at the right 
flightEdited <- flights %>% 
  mutate(date = make_date(year, month, day))
#select the date column
flightEdited %>% 
  select(year, month, day, date) %>% 
  head

#group the date to count the occurence(flights / per day)
daily <- flightEdited %>% 
  group_by(date) %>% 
  summarise(n = n())

#plot the frequency of flights
ggplot(daily, aes(date, n)) + geom_line()

#Show the weekdays and month for the date
updateFlightsEdited <- flightEdited %>% 
  mutate(weekday = wday(date, label = TRUE)) %>% 
  mutate(month = month(date, label = TRUE))

# 1.1exercise ----------------------------------------------------------------
#filter Janunary
Jan <- flightEdited %>% 
  filter(month == "1") %>% 
  group_by(date) %>% 
  summarise(number.flights = n())
#plot the flight in January
ggplot(Jan, aes(date, number.flights)) + geom_line()

# #1.2 exercise to find the most flights by weekday and month -------------
 mostFlights <- flightEdited %>% select(day, month, date) %>% 
  group_by(date,month) %>% 
  summarise(count = n()) %>% 
  group_by(month) %>%
     summarise(sum = sum(count)) %>% 
     filter(sum == max(sum))

# 2.Creating a correlation matrix -------------------------------------------
data(mtcars)
str(mtcars)
#apply cor() can find the relationship between each pair
mcor <- cor(mtcars)
round(mcor, digits = 2) #adjust the digits 

# 2.1 corrplot package ----------------------------------------------------

install.packages("corrplot")
library(corrplot)
corrplot(mcor)


