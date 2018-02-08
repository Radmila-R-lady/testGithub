#########Practising dplyr package
install.packages("hflights")
#loading libraries 
library(dplyr)
library(hflights)

#explore data
data(hflights)
head(hflights)

#converting data frame to a LOCAL data frame for better printing using tbl_df
hflights<-tbl_df(hflights)
hflights  #now when printed looks much nicer

#######GRAMMAR OF DPLYR ###########
######   filter      #keeps rows matching the criteria
# base R approach to view all flights on January 1
hflights[hflights$Month==1 & hflights$DayofMonth==1, ]

# dplyr approach
# note: you can use comma or ampersand to represent AND condition
filter(hflights, Month==1, DayofMonth==1)

# use pipe for OR condition 
filter(hflights, UniqueCarrier=="AA" | UniqueCarrier=="UA")

#the same as above without writing the variable twice
filter(flights, UniqueCarrier %in% c("AA", "UA"))


# base R approach to select DepTime, ArrTime, and FlightNum columns
flights[, c("DepTime", "ArrTime", "FlightNum")]
# dplyr approach
select(flights, DepTime, ArrTime, FlightNum)

# use colon to select multiple contiguous columns, and use `contains` to match columns by name
# note: `starts_with`, `ends_with`, and `matches` (for regular expressions) can also be used to match columns by name
select(flights, Year:DayofMonth, contains("Taxi"), contains("Delay"))


#chaining or pipelining
# nesting method to select UniqueCarrier and DepDelay columns and filter for delays over 60 minutes
filter(select(flights, UniqueCarrier, DepDelay), DepDelay > 60)

# chaining method   %>% is read as then
flights %>%
  select(UniqueCarrier, DepDelay) %>%
  filter(DepDelay > 60)

#Chaining increases readability significantly when there are many commands
#Operator is automatically imported from the magrittr package
#Can be used to replace nesting in R commands outside of dplyr

#mutate
# base R approach to create a new variable Speed (in mph)
flights$Speed <- flights$Distance / flights$AirTime*60
flights[, c("Distance", "AirTime", "Speed")]

# dplyr approach (prints the new variable but does not store it)
flights %>%
  select(Distance, AirTime) %>%
  mutate(Speed = Distance/AirTime*60)

#store the new variable
flights <- flights %>% mutate(Speed = Distance/AirTime*60)


# dplyr approach: create a table grouped by Dest, and then summarise each group by taking the mean of ArrDelay
hflights %>%
  group_by(Dest) %>%
  summarise(avg_delay = mean(ArrDelay, na.rm=TRUE))

#https://www.youtube.com/watch?v=jWjqLW-u3hc
