###########################################
# Project: Week 11 Lab 
# Purpose: Cleaning + Regressions in R
# Author: Alexis Hancz
# Edit date: Nov 16, 2021
# Data: ssowsinclusion
###########################################
install.packages("tidyverse")
library (tidyverse)
install.packages("car")
library (car)
install.packages('odds.n.ends') 
library(odds.n.ends)

#read in data
data<- read.csv('data.csv', header=TRUE)

#create new Gender variable 
#label label Gender values Male and Female instead of 1 and 2
data$Gender <- recode(data$Gender, "1='M'; 2='F'")
#label Gender values Male and Female instead of 1 and 2

# get counts of CD1 and Gender variables 
table(data$CD1)
table(data$Gender)

#check the data type of CD1 and Gender variables 
class(data$Gender)
class(data$CD1)

# get proportion of respondents by Gender
table1<- table(data$Gender) #save table as object first 
table1
prop1<- prop.table(table1) #then run prop.table() on the object
prop1


# get summary of stats by Age
summary(data$age)

#run a linear regression comapring age and gender 
lm<- lm(inclusion6 ~ age + Gender, data=data)
summary(lm)

#define new "labor" variable, switch 1s and 2s with laborforce
data$labor <- (data$laborforce)
#change data type of labor to factor 
data$labor <- as.factor(recode(data$labor, "1='2'; 2='1'"))
as.factor(data$labor)

#check the data type of labor variable
class(data$labor)


#check counts
table(data$laborforce)
table(data$labor)

# run binomial logistic regression (dichotomous dependent variable)
glm<- glm(data$labor ~ data$age + data$Gender, binomial("logit"))
summary(glm)

#run a linear regression comparing age and gender 
lm<- lm(inclusion6 ~ age + Gender, data=data)

#find odds ratios of the model
odds.n.ends(glm)

