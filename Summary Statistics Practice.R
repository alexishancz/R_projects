##########################################################
# Project: Lab 10
# Purpose: Practice summary statistics
# Author: Alexis Hancz 
# Edit date: Nov 4 2022
# Data: Lyrics data
##########################################################

#removed wrong name dataset
rm(LyricsandTask1.csv)
#read in renamed dataset
Lyrics.data.csv <- read.csv(file.choose(), header = T)
#installation of dplyr and janitor packages
install.packages("dplyr") 
install.packages("janitor")
library(dplyr)
library(janitor)
library(tidyverse)

#see how R read the data and what data types were assigned
summary(Lyrics.data.csv)
#shows data type 
glimpse(Lyrics.data.csv)

#rename cleaned dataset
Lyrics.data.cleaned.csv <- Lyrics.data.csv %>% 
#recode the data type for variables incorrectly assigned 
  mutate(Condition = as.factor(x = Condition))%>% 
  mutate(Gender = as.factor(x = Gender))%>% 
  mutate(Year = as.factor(x = Year))%>%
  mutate(LyricsOnly = as.factor(x = LyricsOnly))%>%
  mutate(Pieces = as.numeric(x = Pieces))%>%
#select Condition, Pieces, Sex, Year, LyricsOnly to be included in the dataframe 
  select(Condition, Pieces, Gender, Year, LyricsOnly)
  
#see if R cleaned the data
  summary(Lyrics.data.csv)
  glimpse(Lyrics.data.csv)
  
#Create new Percent variable
Lyrics.data.cleaned.csv$Percent <- (Lyrics.data.cleaned.csv$Pieces)/25*100

#run mean, median, variance, sd, and range for Percent 
mean(x = Lyrics.data.cleaned.csv$Percent, na.rm = TRUE)
median(x = Lyrics.data.cleaned.csv$Percent, na.rm = TRUE)
var(x = Lyrics.data.cleaned.csv$Percent, na.rm = TRUE)
sd(x = Lyrics.data.cleaned.csv$Percent, na.rm = TRUE)
range(x = Lyrics.data.cleaned.csv$Percent, na.rm = TRUE)

#run mean, median, variance, sd, and range for Pieces
mean(x = Lyrics.data.cleaned.csv$Pieces, na.rm = TRUE)
median(x = Lyrics.data.cleaned.csv$Pieces, na.rm = TRUE)
var(x = Lyrics.data.cleaned.csv$Pieces, na.rm = TRUE)
sd(x = Lyrics.data.cleaned.csv$Pieces, na.rm = TRUE)
range(x = Lyrics.data.cleaned.csv$Pieces, na.rm = TRUE)

#run freq distributions for Condition, Gender, Year, LyricsOnly
frequency(x = Lyrics.data.cleaned.csv$Condition, plot = FALSE)
frequency(x = Lyrics.data.cleaned.csv$Gender, plot = FALSE)
frequency(x = Lyrics.data.cleaned.csv$Year, plot = FALSE)
frequency(x = Lyrics.data.cleaned.csv$LyricsOnly, plot = FALSE)

#getting means by group- Pieces by Condition
Lyrics.data.cleaned.csv %>%                                        
  group_by(Condition) %>%                         
  summarise_at(vars(Pieces),             
               list(name = mean))

#getting std devs by group- Pieces by Condition
Lyrics.data.cleaned.csv %>%                                        
  group_by(Condition) %>%                         
  summarise_at(vars(Pieces),             
               list(name = sd)) 

#ANOVA
# Compute the analysis of variance looking at avg Pieces for each of the Conditions
ConditionPiecesaov <- aov(Pieces ~ Condition, data = Lyrics.data.cleaned.csv)
# Summary of the analysis
summary(ConditionPiecesaov)

#getting means by group- Pieces by LyricsOnly
Lyrics.data.cleaned.csv %>%                                        
  group_by(LyricsOnly) %>%                         
  summarise_at(vars(Pieces),             
               list(name = mean))

#getting std devs by group- Pieces by LyricsOnly
Lyrics.data.cleaned.csv %>%                                        
  group_by(LyricsOnly) %>%                         
  summarise_at(vars(Pieces),             
               list(name = sd)) 

#T-TEST
#Run t-test comparing Pieces by LyricsOnly
TTESTPIECESLYR <- t.test(formula = Lyrics.data.cleaned.csv$Pieces ~
                        Lyrics.data.cleaned.csv$LyricsOnly)
TTESTPIECESLYR

#gender by condition crosstab
#load function that creates Column percentages with crosstab instruction
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
crosstab(Lyrics.data.cleaned.csv, row.vars = "Gender", col.vars = "Condition", type = "c")

#year by condition crosstab
#load function that creates Column percentages with crosstab instruction
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
crosstab(Lyrics.data.cleaned.csv, row.vars = "Year", col.vars = "Condition", type = "c")
