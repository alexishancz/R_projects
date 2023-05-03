######################################################################################################
#HANDS-ON SESSION 8

#Use the ‘flights’ dataset to perform the following tasks.
######################################################################################################
#1. ‘dep delay’ column shows the flight departure delay (in minutes). Using the group by and
#summarize functions, find the average departure delay on each day.

flights_data <- read.csv("flights.csv") #reading in the CSV

library(dplyr) #loading dyplr library

colnames(flights_data) #viewing the names of columns 

avg_delay_by_day <- flights_data %>%  #piping the data to the dplyr pipeline
  group_by(day) %>%  #grouping the data by day
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE))  #calculating the mean departure delay for each day excluding missing values

avg_delay_by_day  #view results

#2. Using mutate function, create three new columns: ‘gain’, ‘hours’ and ‘gain per hour’. 
#‘gain’ is the difference between arrival delay (arr delay) and departure delay (dep delay), 
#‘hours’ is arrival time (arr time) divided by 60, while ‘gain per hour’ is ‘gain’ divided by ‘hours’.
flights_data <- flights_data %>% #piping the data to the mutate function
  mutate(gain = arr_delay - dep_delay, #creating 'gain' column subtractig dep_delay from arr_delay
         hours = arr_time / 60,        #creating 'hours' column dividing arr_time by 60
         gain_per_hour = gain / hours) #creating 'gain_per_hour' column dividing gain by hours
head(flights_data) #peek at the updates in the dataframe


#3. Create a new column from the ‘hours’ column and call it ‘duration’. 
#For flights below 3 hours, let the duration be ‘short flight’, and for flights 3 hours and above, let the duration be ‘long flight’.
flights_data <- flights_data %>% #piping the dataframe to the mutate function
  mutate(duration = ifelse(hours < 3, "Short Flight", "Long Flight")) #specifying duration categories using ifelse statement within the mutate function
head(flights_data) #peek at the updates in the dataframe


#4. How many flights are ‘short flight’ and how many flights are ‘long flight’.
table(flights_data$duration)

#short flight: 9465
#long flight: 318598







