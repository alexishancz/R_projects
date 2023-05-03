## R_Challenge_analysis_ACLED.R
##########################################################################
## INSTRUCTOR: Christopher Fariss
##
## COURSE NAME: Data Science for International Studies (DSIS)
## University of Michigan, Winter 2023, Winter 2022, Winter 2021, Winter 2020
##
## COURSE NAME: Advanced Computational Methods for Social Media and Textual Data (2F)
## University of Essex Summer School 2022, 2021, 2020
##
## Date: 2023-01-20
##
## Please e-mail me if you find any errors or have and suggestions (either email is fine)
## e-mail: cjf0006@gmail.com
## e-mail: cjfariss@umich.edu
##########################################################################
#only have to do up to 8c. graph and text analysis can be separate program challenges 

## Instructions:
##
## These challenges are meant to be just that, challenging. They should also be fun. I encourage you to think creatively and collaboratively. Getting stuck or not finishing all the steps is expected and encouraged. This is how learning works.
##
## Always start with step (1) and then continue to each step as time permits.
## Don't worry about completing each step. Document your code for each step.
## You may wish to come back to some of the harder steps as you progress through the course.
## Note that some of the steps may ask you to use skills we have not yet covered in the course.
## Don't worry about these steps now but definitely think through the programming logic if you are stuck and make plans to come back to try them once you feel ready.
##
##########################################################################
##
## Steps for the Challenge
##
## Extract event data from ACLED API at: https://www.acleddata.com/
## Note that because of changes to the ACLED website, I've now provided 3 of the curated datasets from ACLED in the Canvas course website.
## You're welcome to register at ACLED and download additional datasets if you are interested.

## (1) Load your ACLED dataset into R.

acled_data <- read.csv("acled_data_Afghanistan.csv")

## (2) Inspect and explore the ACLED data.
dim(acled_data) #getting the number of rows and columns
names(acled_data) #getting the column names 
head(acled_data) #getting the first 6 rows of data
table(acled_data$ADMIN1) #getting a table of the total # of events for each first order administrative unit in Afghanistan
str(acled_data) #getting the structure of the dataset

#getting a bar plot of event frequencies from the EVENT_TYPE column 
tab_out <- table(acled_data$EVENT_TYPE) #creating a table of the frquency of each event type  
par(mar=c(4,6,2,.5)) #setting the margins of the plot using plot parameters par() function 
barplot(tab_out[order(tab_out)], horiz=TRUE, las=2, cex.names=.3) #creating the bar plot; tab_out argument sorts frequencies in ascending order, horiz=TRUE specifies a horizontal bar plot; las=2 changes the orientation of the x axis labels; cex.names=.3 sets the font size for the labels 

## (2a) Specifically, how many rows and columns are contained in the dataset?

#find the number of rows and columns in the dataset
nrow(acled_data) #getting the number of rows: 64795
ncol(acled_data) #getting the number of columns: 34

## (2b) What variable type are each of the variables?
##Out of 31 variables, most (19) variables are characters, then integers (9). Numeric and other make up just three of the variables. 

test <- c() #creating an empty vector 
for(i in 1:ncol(acled_data)) { #setting up a loop to iterate over each column 
  if(is.factor(acled_data[,i])) { #checking the data type of each column with an if-else statement
    test[i] <- "factor" #assigns the column to "factor" in the resulting vector 
  } else if(is.numeric(acled_data[,i])) {
    if(is.integer(acled_data[,i])) {
      test[i] <- "integer" #assigns the column to "integer" in the resulting vector 
    } else {
      test[i] <- "numeric" #assigns the column to "numeric" in the resulting vector 
    }
  } else if(is.character(acled_data[,i])) {
    test[i] <- "character" #assigns the column to "character" in the resulting vector 
  } else {
    test[i] <- "other" #assigns the column to "character" in the resulting vector if it's any other variable type
  }
}
test

table(test) #view results as a frequency table


## (2c) What are some descriptive statistics for the variables (i.e. summary())?

##the average # of fatalities is about 3.1, and it ranges from 0 to 184. Most often, events experience no fatalities (Q1=0, median= 0):
summary(acled_data)

##most variables are characters or factors, for which measures of central tendency are not accurate descriptors of the data; so I'll use a crosstab. 
##36 instances of violence against civilians are associated with NATO; 28 instances are associated with U.S. NATO military forces, 2017-2021; 24 instances are associated with U.S. military forces:
xtab <- xtabs(~ acled_data$ASSOC_ACTOR_1 + acled_data$EVENT_TYPE) #using a crosstab to interpret some factor and character variables 


## (3a) subset() the data to focus on 1 country (use your country from the case selection essay)

#Because my CSV is only Afghanistan data, I am subsetting the data to focus on one location, Kabul. (we emailed about this)

kabul_location <- subset(acled_data, LOCATION == "Kabul") #subsetting the data to just get events in Kabul 
table(kabul_location$EVENT_TYPE) #getting the frequency breakdown of each event type in Kabul

## (3b) Inspect and explore the subset of the original ACLED data

str(kabul_location) #checking the structure of the dataframe 
head(kabul_location) #viewing the first few rows of data 
summary(kabul_location) #summary statistics for numeric variables

##exploring the interaction column: 
##the three most common interaction codes are 12, 37 and 60, which represent "Military Vs. Rebels", "Political Militia vs. Civilians", and "Sole Protester Action", respectively.

kabul_location$INTERACTION <- as.factor(kabul_location$INTERACTION) #converting the column to factor because it is a categorical variable 
str(kabul_location$INTERACTION) #checking to see if it worked 
table(factor(kabul_location$INTERACTION)) #using the table() function to get the frequency of each interaction code
interaction_prop <- prop.table(table(kabul_location$INTERACTION)) * 100 #multiplying by 100 to get the frequencies as percentages 
interaction_prop_sorted <- sort(interaction_prop, decreasing = TRUE) #sorting the interacton proportion table by the frequency of each interaction code


## (4) What is the proportion of EVENT_TYPE for your country?
##the three most common events in Kabul are Explosions/Remote violence (32.75%), Violence agains civilians (20.25%), and Protests (19.12%). Battles come in at a close 4th at 18.18%. 
event_freq_table <- table(kabul_location$EVENT_TYPE) #creating a frequency table by event type for Kabul 
event_prop_table <- prop.table(event_freq_table) #using the prop.table() function to get the proportion of each event type
event_percent_table <- event_prop_table * 100 #multiplying by 100 to get the percentages 


## (5) Pick one common event in your country. What is the proportion of this EVENT_TYPE in all of the other countries in the dataset?
##picking Violence against civilians to compare the proportion in Kabul to all other regions in Afghanistan: 

acled_data$violciv <- ifelse(acled_data$EVENT_TYPE=="Violence against civilians", 1, 0) #creating new binary variable column in the original dataset to identify if the event type is violence against civilians or not 
table(acled_data$violciv)/length(acled_data$violciv) #calcing the proportion of the binary variable, which is 0.056 or 5.6%
var(acled_data$violciv) #calcing the variance of the binary variable, which is 0.053

## (6) For just your country, what is the proportion of each of the EVENT_TYPE across the three curated datasets?
#n/a; only working from one Afghanistan dataset (we emailed about this)

## (7a) Calculate the variance of your EVENT_TYPE
variance <- var(acled_data$violciv) #calculating variance in frequency of violence against civilians  = 0.053

## (7b) Calculate the standard deviation of your EVENT_TYPE

sd(acled_data$violciv) #calcing sd, which is 0.23 

## (8a) Calculate the correlation coefficient for the EVENT_TYPE you selected with one of the other variables or other events in the dataset
##I am estimating the correlation coefficient for violence against civilians with the number of fatalities. 
##There is a negative correlation between violence against civilians and # of fatalities (Pearson coeff = -0.075). 
##As the freq of violence against civilians increases, we can expect the number of fatalities decrease by 2.14 units. 
##Higher frequency of violence against civilians is associated with lower number of fatalities. this COULD mean that the violence is less severe/lethal in nature. 

#Pearson coefficient: -0.075 (strong negative relationship)
cor(acled_data$FATALITIES, acled_data$violciv) #calculating correlation using cor() function


## (8b) Estimate a linear model using your EVENT_TYPE as the left-hand-side variable with one of the other variables or other events in the dataset
#I will be comparing the results of a linear regression and a logistic regression for the sake of exploration. 

acled_data$explo <- ifelse(acled_data$EVENT_TYPE=="Explosions/Remote violence", 1, 0) #creating a binary variable for explosions / remote violence to use in the regression

##There is a slightly negative correlation between violence against civilians and explosions / remote violence (regression coeff = -0.076749). 
##This means that for every one unit increase in the frequency of explosions/remote violence, the frequency of violence against civilians is expected to decrease by -0.07. 
##I am surprised that the relationship is negative. One explanation could be that using remote violence is associated with a decrease in on-the-ground violence against civilians. 

#Linear regression: 
my_model_1 <- lm(acled_data$violciv ~ acled_data$explo, data=acled_data)
summary(my_model_1)


##This model produces the same result as the linear regression model. 
#Logistic regression: 
my_model_1 <- glm(acled_data$violciv ~ acled_data$explo, data=acled_data)
summary(my_model_1)

## (8c) Estimate a linear model using your EVENT_TYPE as the left-hand-side variable with several variables
#I will be comparing the results of a linear regression and a logistic regression for the sake of exploration. 

#estimating a model for violence against civilians (dependent) with whether or not the ASSOC_ACTOR_1 is NATO (independent) and the year (indepdendent)
acled_data$nato <- ifelse(acled_data$ASSOC_ACTOR_1=="NATO: North Atlantic Treaty Organization; Military Forces of the United States (2017-2021)", 1, 0)

##In this model, there is a slightly negative correlation (coeff= -0.076) between violence against civilians and explosions & remote violence. 
##This means that for every one unit increase in the frequency of explosions/remote violence, the frequency of violence against civilians is expected to decrease by -0.076. 
##There is a positive correlation (coeff= 0.34) between violence against civilians and whether or not the associated actor is NATO. 
##This means that when NATO involved actor in an event, violence against civilians, the frequency of violence against civilians is expected to increase by 0.34. 

#Linear Regression:
my_model_2 <- lm(acled_data$violciv ~ acled_data$explo + acled_data$nato, data = acled_data, family = "binomial")
summary(my_model_2)


##In this model, there is a heavy-magnitude negative relationship between violence against civilians and explosions & remote violence (coeff = -17.07).
##This means that for every one unit increase in the frequency of explosions/remote violence, the frequency of violence against civilians is expected to decrease by -17.07. 
##There is a positive correlation (coeff= 2.42) between violence against civilians and whether or not the associated actor is NATO. 
##This means that when NATO involved actor in an event, violence against civilians, the frequency of violence against civilians is expected to increase by 2.42. 

#Logistic Regression:
my_model_2 <- glm(acled_data$violciv ~ acled_data$explo + acled_data$nato, data = acled_data, family = "binomial")
summary(my_model_2)


## Warning: The rest of these steps are more challenging:
## 
## GRAPHS:
## (Note we will cover a little bit more about working with text-as-data in 2 weeks)
##
## (9) Make a barplot of the proportion of EVENT_TYPE for your country from each of the three datasets included in this challenge

#installing necessary packages
install.packages("ggplot2")
library("ggplot2")
install.packages("magrittr")
library("magrittr")
EVENT_TYPE <- acled_data %>% #piping "data" dataframe through the count of diet quality (count() function)
  count(EVENT_TYPE) %>%      
  mutate(prop = n / sum(n)) #piping the count of diet quality through the mutate() function to create a new column representing the proportion of each level

plot1 <- ggplot(EVENT_TYPE, aes(x = EVENT_TYPE, y = prop, fill = EVENT_TYPE)) + #creating a plot object with new diet_quality dataframe using ggplot(); specifying input dataframe, x and y variables, and that diet quality will determine color levels represented in the graph
  geom_bar(stat = "identity") + #creating a bar 
  scale_fill_brewer(palette = "Set1") + #setting the color palette of the bars using scale_fill_brewer()
  labs(x = "Event Type", y = "Proportion", #labeling x, y, title using labs() function 
       title = "Proportion of Event Types in Afghanistan ") +
  theme_minimal() #setting the theme to a minimal style 

ggsave("plot1.pdf", plot = plot1) #saving plot4 as pdf using ggsave() function 

## (10) How many times does the EVENT_TYPE you selected occur in each year of data (2018 - 2020)?
##2018: 332 counts; 2019: 405 counts; 2020: 712 counts 
library(dplyr) 

acled_data %>% #piping dataframe to the next function
  filter(EVENT_TYPE == "Violence against civilians") %>% #filtering for specific event type
  group_by(YEAR) %>% #grouping the filtered data by year
  summarise(count = n()) #summarizing the counts of event type for each year

## (11) Create a time series plot for the frequency of each EVENT_TYPE for all countries each week of the year
#this is my imperfect attempt at creating a time series plot; I'm not sure if I did it right and I'm having trouble opening the PDF file I saved it to. 

#installing the necessary packages
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("lubridate")
library(lubridate)

# convert the EVENT_DATE column to a standard format
acled_data <- acled_data %>% #piping acled data to mutate function
  mutate(EVENT_DATE = as.Date(EVENT_DATE, format = "%m/%d/%Y")) #mutate the EVENT_DATE column into a standard date time format


# create a new data frame with weekly counts of event types by location
acled_weekly <- acled_data %>%
  mutate(week = week(EVENT_DATE)) %>% #extracting the week from the mutated EVENT_DATE column 
  group_by(LOCATION, week, EVENT_TYPE) %>% #grouping the data by location, week and event type
  summarize(count = n()) %>% #getting frequencies using the summarize() function 
  ungroup()

# create the time series plot
time_series_plot <- ggplot(acled_weekly, aes(x = week, y = count, color = EVENT_TYPE)) + #creating the plot using ggplot; specifying data, x,y, colors
  geom_line() + #specifies a line graph plot
  facet_wrap(~ LOCATION, ncol = 2) + #creates a separate plot for each unique location
  scale_x_continuous(breaks = 1:52, minor_breaks = NULL, expand = c(0, 0)) + #sets the x axis breaks to every week
  labs(x = "Week of Year", y = "Frequency", color = "Event Type") + #setting labels and color legend 
  theme_bw() #setting theme

ggsave("time_series_plot.pdf", time_series_plot) #saving as a PDF using ggsave 

####

## TEXT ANALYSIS:
## (Note we will cover a little bit more about working with text-as-data in 1 week)
##
## (12) Turn the event description in the ACLED dataset into a Document-by-Term Matrix (DTM). For the DTM, we let i = 1, ..., N index documents and w = 1, ..., W index the unique terms in the collection of documents. For each of the i documents, we determine the frequency of each of the unique $w$ words. Each of the D_iw entries in a DTM represents  the number of times the w-th word appears in the i-th document.

library(tidytext)
library(dplyr)

#preprocessing 
install.packages("tm") #installing tm package 
library('tm')
mycorpus <- Corpus(VectorSource(acled_data$NOTES)) #creating a corpus from the notes column
mycorpus <- tm_map(mycorpus, removePunctuation) #removing punctuation from my corpus 
mycorpus <- tm_map(mycorpus, content_transformer(tolower)) #converting all text to lowercase using tolower() function
mycorpus <- tm_map(mycorpus, removeWords, stopwords("english")) #removing english stopwords from my corpus 

# create document-term matrix
mydtm <- DocumentTermMatrix(mycorpus) #creating a DTM with the DTM function, specifying my corpus data 


## (13) Which words are most commonly associated with the EVENT_TYPE you selected for the entire dataset?

#The top 10 most commonly associated words with Violence Against Civilians are: 
#killed, taliban, district, reported, city, forces, province, area, civilian, unknown. 

#finding rows containing my event type and saving as a new object 
violciv_rows <- which(acled_data$EVENT_TYPE == "Violence against civilians")

#subsetting the DTM using the event type object I just created, saving as new object 
violciv_dtm <- mydtm[violciv_rows, ]

#finding the most frequent terms in the subsetted matrix
violciv_freq_terms <- findFreqTerms(violciv_dtm, lowfreq = 50, highfreq = Inf)

#calculating the frequency of each term in the subsetted matrix
violciv_term_freq <- apply(violciv_dtm[, violciv_freq_terms], 2, sum)

#sorting the term frequencies in descending order and printing the top 10 terms
top_terms <- head(sort(violciv_term_freq, decreasing = TRUE), 10)
print(top_terms)


## (14) Create a barplot of the most frequent words by EVENT_TYPE for the entire dataset
#In order to succeed, I will make a separate barplot of the top 10 most frequent words for each event type. 

#Violence against civilians 
#create a barplot of the frequencies for Violence against civilians 
barplot_1 <- barplot(top_terms, names.arg = c("killed", "taliban", "district", "reported", "city", "forces", "province", "area", "civilian", "unknown"), xlab = "Term", ylab = "Frequency", main = "Top 10 Terms Associated with Violence Against Civilians")


#Battles 
#finding rows containing my event type and saving as a new object 
battles_rows <- which(acled_data$EVENT_TYPE == "Battles")

#subsetting the DTM using the event type object I just created, saving as new object 
battles_dtm <- mydtm[battles_rows, ]

#finding the most frequent terms in the subsetted matrix
battles_freq_terms <- findFreqTerms(battles_dtm, lowfreq = 50, highfreq = Inf)

#calculating the frequency of each term in the subsetted matrix
battles_term_freq <- apply(battles_dtm[, battles_freq_terms], 2, sum)

#sorting the term frequencies in descending order and printing the top 10 terms
top_terms_2 <- head(sort(battles_term_freq, decreasing = TRUE), 10)
print(top_terms_2)

#creating a barplot of the frequencies
barplot_2 <- barplot(top_terms_2, names.arg = c("taliban", "killed", "militants", "district", "afghan", "forces", "fatalities", "reported", "province", "wounded"), xlab = "Term", ylab = "Frequency", main = "Top 10 Terms Associated with Battles")

#Explosions/Remote violence 
#finding rows containing my event type and saving as a new object 
explo_rows <- which(acled_data$EVENT_TYPE == "Explosions/Remote violence")

#subsetting the DTM using the event type object I just created, saving as new object 
explo_dtm <- mydtm[explo_rows, ]

#finding the most frequent terms in the subsetted matrix
explo_freq_terms <- findFreqTerms(explo_dtm, lowfreq = 50, highfreq = Inf)

#calculating the frequency of each term in the subsetted matrix
explo_term_freq <- apply(explo_dtm[, explo_freq_terms], 2, sum)

#sorting the term frequencies in descending order and printing the top 10 terms
top_terms_3 <- head(sort(explo_term_freq, decreasing = TRUE), 10)
print(top_terms_3)

#creating a barplot of the frequencies
barplot_3 <- barplot(top_terms_3, names.arg = c("district", "killed", "taliban", "afghan", "militants", "province", "forces", "reported", "wounded", "fatalities"), xlab = "Term", ylab = "Frequency", main = "Top 10 Terms Associated with Explosions and Remote Violence")


#Strategic developments
#finding rows containing my event type and saving as a new object 
strat_rows <- which(acled_data$EVENT_TYPE == "Strategic developments")

#subsetting the DTM using the event type object I just created, saving as new object 
strat_dtm <- mydtm[strat_rows, ]

#finding the most frequent terms in the subsetted matrix
strat_freq_terms <- findFreqTerms(strat_dtm, lowfreq = 50, highfreq = Inf)

#calculating the frequency of each term in the subsetted matrix
strat_term_freq <- apply(strat_dtm[, strat_freq_terms], 2, sum)

#sorting the term frequencies in descending order and printing the top 10 terms
top_terms_4 <- head(sort(strat_term_freq, decreasing = TRUE), 10)
print(top_terms_4)

#creating a barplot of the frequencies
barplot_4 <- barplot(top_terms_4, names.arg = c("taliban", "district", "forces", "reported", "province", "afghan", "around", "militants", "2021", "2020"), xlab = "Term", ylab = "Frequency", main = "Top 10 Terms Associated with Strategic Developments")

#Protests
#finding rows containing my event type and saving as a new object 
protest_rows <- which(acled_data$EVENT_TYPE == "Protests")

#subsetting the DTM using the event type object I just created, saving as new object 
protest_dtm <- mydtm[protest_rows, ]

#finding the most frequent terms in the subsetted matrix
protest_freq_terms <- findFreqTerms(protest_dtm, lowfreq = 50, highfreq = Inf)

#calculating the frequency of each term in the subsetted matrix
protest_term_freq <- apply(protest_dtm[, protest_freq_terms], 2, sum)

#sorting the term frequencies in descending order and printing the top 10 terms
top_terms_5 <- head(sort(protest_term_freq, decreasing = TRUE), 10)
print(top_terms_5)

#creating a barplot of the frequencies
barplot_5 <- barplot(top_terms_5, names.arg = c("protest", "city", "province", "reported", "kabul", "taliban", "protested", "women", "protesters", "2022"), xlab = "Term", ylab = "Frequency", main = "Top 10 Terms Associated with Protests")


#Riots 
#finding rows containing my event type and saving as a new object 
riots_rows <- which(acled_data$EVENT_TYPE == "Riots")

#subsetting the DTM using the event type object I just created, saving as new object 
riots_dtm <- mydtm[riots_rows, ]

#finding the most frequent terms in the subsetted matrix
riots_freq_terms <- findFreqTerms(riots_dtm, lowfreq = 50, highfreq = Inf)

#calculating the frequency of each term in the subsetted matrix
riots_term_freq <- apply(riots_dtm[, riots_freq_terms], 2, sum)

#sorting the term frequencies in descending order and printing the top 10 terms
top_terms_6 <- head(sort(riots_term_freq, decreasing = TRUE), 10)
print(top_terms_6)

#creating a barplot of the frequencies
barplot_6 <- barplot(top_terms_6, names.arg = c("taliban"), xlab = "Term", ylab = "Frequency", main = "Top 10 Terms Associated with Riots")

##########################################################################