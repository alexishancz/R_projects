##########################################################
# Project: Lab 9
# Purpose: Learn basic R functions 
# Author: Alexis Hancz
# Edit date: Oct 30 2022
# Data: Music data 
##########################################################

#install tidyverse, janitor and dplyr packages, read using library command 
library(package = "tidyverse")
library(package="janitor")
library(dplyr)

#import the CSV file into R as a new project called music.data
music.data <- read.csv("music.data.csv", header = T)

#using the piping function, clean data and rename the data set to music.data.cleaned
music.data.cleaned <- music.data %>%
#rename the variables as Gender, Country, Hours, Platform.
  rename(Gender=Sex, Country=Country.Music, Hours=Hours.per.day.listening.to.Music, Platform=Favorite.Music.Streaming.Platform)%>%
#recode the data type for variables incorrectly assigned 
  mutate(Gender = as.factor(x = Gender))%>% 
  mutate(Platform = as.factor(x = Platform))%>% 
  mutate(Country = as.numeric(x = Country))%>% 
#label Gender values Male and Female instead of 1 and 2
  mutate(Gender= recode(.x = Gender, "1" = "Male", "2" = "Female"))%>%
#set NA value as not sure for Platform
  mutate(Platform = recode(.x=Platform, "NA" = "Not Sure"))%>% 
#select Gender, Country, Hours Platform to be included in the dataframe 
  select(Gender, Country, Hours, Platform)
#save reduced dataframa as a CSV
write.csv(music.data.cleaned, "MusicHancz.csv", row.names = FALSE)
