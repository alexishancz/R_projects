########################################################################################
#LAB 05- PROBLEM SET #1- DATA MANIPULATION WITH dyplr
########################################################################################
#0. Preliminary Setup

#importing the CSV file and saving it as an object
data <- read.csv("nhanes.csv")

#importing dylpr package for data manipuation 
library(dplyr)

#1. summary stats for numeric variables: mean, median, mode, range, variance, std using summary() function
#   summary stats for categorical variables: frequency, proportion using table() function; 
#   divide by length to get proportion/share

#a. Age (RIDAGEYR)
summary(data$RIDAGEYR)
#paticipants span 20-85 years of age, with the average age being about 47. 25% of participants are under 31
#years old, and 25% of participants are over 61 years old

#b. BMI (BMXBMI)
summary(data$BMXBMI)
#participnts span a wide range of BMIs, from values classified as "underweight" to obsese. For this reason, the
#average may not be an accurate description for this dataset. The median BMI is 27.84, which is within the healthy 
#range. 50% of participants have a BMI between 24.58 and 32.20.

#c.Systolic Blood Pressure (BPXSY1)
summary(data$BPXSY1)
#again, participants span a wide range of sbps. The median sbp is 120, and half the data falls between 110 and 134. 
#there are a large number of missing values for this variable (228). 

#d.Sex (RIAGENDR)
table(data$RIAGENDR)

table(data$RIAGENDR) / length(data$RIAGENDR)
#The large majority of participants are female- about 63%. There are 1302 females and 752 males. 

#e. Race (RIDRETH1)
table(data$RIDRETH1)

table(data$RIDRETH1) / length(data$RIDRETH1)
#white particopants more than double Black pariticpants and almost double Mexican American participants. Other races and other hispanic races
#are small minorities, at 0.042% and 0.038% respectively. 


#f. total cholesterol (LBXTC)
summary(data$LBXTC)
#cholesterol also runs a large range and has a median of 196. 

#g. diet quality (DBQ700)
table(data$DBQ700)

table(data$DBQ700) / length(data$DBQ700)
#interesting. There is no majority, but the largest share of participants rate their diet as good, then very good
#then fair, and both poor and excellent have very small minorities at 5% and 9%, respectively. 

#h. Education Level (DMDEDUC2)
table(data$DMDEDUC2)

table(data$DMDEDUC2) / length(data$DMDEDUC2)
#the largest share of participants have some college (28%), and following close behind is college (25%).
#again, following close behind, 21% of participants are highschool education, and 12.9% and 12.%% have some highschool
#and no highschool, respectively. 


#2. How many people reported no weekly alcohol use?

#1095 people reported no weekly alcohol use: 
WkAlcDays_count <- data %>% count(WkAlcDays == 0)


#3. Create a binary variable with 1 equalling no weekly alcohol use.
#label the categories (O: “alcohol use the past week”; 1: “no alcohol use”)
#double check that no alcohol use matches the original varibale. 

data <- data %>% #using piping function to apply the two functions to the dataframe, ultimately saving weekly_alcohol_label as a new column in the dataframe 
  mutate(weekly_alcohol = ifelse(WkAlcDays == 0, 1, 0)) %>% #writing a conditional statement within a mutate() function: sets weekly alcohol use to 1 if value in original variable =0; if not, assigns 0. connectiing this operation to the following using a piping operator
  mutate(weekly_alcohol_label = ifelse(weekly_alcohol == 1, "No alcohol use", "Alcohol use the past week")) #assigning character labels to new variable outcomes using another conditional statement within a mutate() function


#4. Group the data by the new binary alcohol use variable
#summarize the means of BMI (BMXBMI), Systolic Blood Pressure (BPXSY1), white blood cell count (LBXWBCSI), C-reactive protein (LBXCRP), and homocysteine (LBXHCY).
#What patterns do you see if any? What can’t you say about these patterns?

  #-The mean BMI is surprisingly higher for people who don't drink alcohol than for people who do. 
  #-There's no difference in systolic blood pressure between the two groups.
  #-The non-alcohol group has a higher mean white blood cell count. 
  #-The non-alcohol group has a higher mean c-protein count. 
  #-The alcohol group has a higher mean homocysteine count. 
  #-While these patterns may suggest an association between the health traits and whether or not one drinks alcohol, we cant say that this relationships is causal, nor the magnitude of the
  #strength of the relationship between alcohol consumption and the particular health factor.
  #-Additionally, because the weekly alcohol category is binary, we don't know the variation between drinking habits within the drinking group, which could affect the results drastically. 
  #-Finally, we don't know if there are outliers that are dragging the mean in either direction for any of the health factors. 

#group the data by weekly_alcohol and summarize the means of selected variables
summary_data <- data %>% #using piping function to apply the two functions to the dataframe
  group_by(weekly_alcohol) %>% #using groupby() function with piping function to group the following variable calculations with the new binary weekly alcohol variable 
  summarize(mean_bmi = mean(BMXBMI, na.rm = TRUE), #applying summarize() function to the concatenated variables 
            mean_sbp = mean(BPXSY1, na.rm = TRUE), #removing missing values so R can compute the means
            mean_wbc = mean(LBXWBCSI, na.rm = TRUE), #concatenating each relevant variable to be calculated and grouped 
            mean_cprot = mean(LBXCRP, na.rm = TRUE), #using na.rm logical argument to remove missing values before calculating  
            mean_homo = mean(LBXHCY, na.rm = TRUE)) #using a logical statement to assign names to the calculations

#view the summary data
summary_data


#5. Group the data by diabetes status and the new binary alcohol use variable.
#Then summarize the means of BMI (BMXBMI), Systolic Blood Pressure (BPXSY1), white blood cell count (LBXWBCSI), C-reactive protein (LBXCRP), and homocysteine (LBXHCY).
#What patterns do you see if any? What might be a better way to look at these patterns?
  
  #Some patterns:
  #-The group with the highest mean BMI are those who haven't drank in the past week and are borderline diabetic.
  #-^this mean is surprisingly even higher than for diabetics who drank in the past week. 
  #-Unsurprisingly, The group with the lowest mean BMI and sbp is those who aren't diabetic and havem't drank in the pat week. 
  #-Unsurprisingly, the group with the highest sbp are diabetics who have drank in the past week. 
  #Borderline diabetics and diabetics also generally have a higher mean c-reactive protein, especially if they've drank in the past week. 

  #It would be better to look at these patterns in a crosstab or another kind of graph. 

summary_data2 <- data %>% #using piping function to apply the two functions to the dataframe
group_by(DIQ010, weekly_alcohol_label) %>% #specifying the columns to group by, by diabetes and alcohol use, with groupby(); piping with the following function
  summarize(mean_bmi = mean(BMXBMI, na.rm = TRUE),  #applying summarize() function to the concatenated variables 
            mean_sbp = mean(BPXSY1, na.rm = TRUE), #removing missing values so R can compute the means
            mean_wbc = mean(LBXWBCSI, na.rm = TRUE), #concatenating each relevant variable to be calculated and grouped 
            mean_cprot = mean(LBXCRP, na.rm = TRUE), #using na.rm logical argument to remove missing values before calculating 
            mean_homo = mean(LBXHCY, na.rm = TRUE)) #using a logical statement to assign names to the calculations
 
summary_data2

########################################################################################
#LAB 05- PROBLEM SET #2- R PLOTS USING baseR lattice package or ggplot
########################################################################################

#1. Create a scatterplot matrix of the outcomes white blood cell count (LBXWBCSI), C-reactive protein (LBXCRP), and homocysteine (LBXHCY). 
#What can you infer from this graph?

  #There seems to  be a lossely correlated relationship between c-reactive protein and white blood cell count; a small group of outliers increase in 
  #white blood cell count as c-reactive protein increases. None of the scatterplots in the matrix seem to hold a strong positve relationship between any two variable combinatins,
  #but we see this loose positive trend between homocysteine and c-reactive protein as well. 

#loading the lattice package to create the scatterplot matrix
library(lattice)

#subsetting the data to include only the variables of interest
data_subset <- data[, c("LBXWBCSI", "LBXCRP", "LBXHCY")] #concatenating a vector of the relevant variables with c() function to include and assigning them to a subset object 

#create the scatterplot matrix using lattice splom () function
splom(data_subset) #splom takes the subsetdataframe as input to create a scatterplot matrix


#2. Label all axes appropriately and title the plot. What does this graph show?

  #The bar chart shows participant age distribution, broken down by sex. As reflected by the summary statistics earlier, we can see that females dominate 
  #the participant pool, with the majority of the graph being pink. We can also see that the greatest share of female
  #participants are 25-30 years old, and the greatest share of male participants are 35-40 years old. 
  #The greatest share of participants overall are 25-30 years old. 

#loading the ggplot2 package to use to create a histogram
library(ggplot2)

#creating the histogram using ggplot
plot2 <- ggplot(data = data, aes(x = RIDAGEYR, fill = factor(RIAGENDR))) + #initializing a ggplot object "plot2"; specifying "data" as the dataframe for the plot; using aes() function for visual properties, mapping age to the x axis of the plot, mapping gender to fill the colors of the bars; 
  #factor() function ensures gender is treated as a categorical variable; + symbol makes R wait for additional modifications 
  geom_histogram(alpha = 0.5, position = "stack", bins = 20) + #adding a histogram layer w/ transparency 0.5, stacked bar position, and 20 bins
  labs(title = "Age Distribution by Sex", x = "Age", y = "Frequency") + #adding title, x and y labels using labs() function
  scale_fill_discrete(name = "Sex") #displaying a color corresponding to each sex + a legend using scale_fill_discrete() function for categorical variables 

ggsave("plot2.pdf", plot = plot2) #saving plot2 as pdf using ggsave() function 


#4. 
# Create the scatterplot using ggplot
#What does this graph show?

  #The scatterplot compares pariticpant age to participant BMI. 
  #It shows that the age range of the dataset encompasses a diverse range of BMIs at any age;
  #however, it stands out that BMIs from 30-60 years of age contain more higher-than-avergae-BMI outliers than <30 y/o's and >70 y/o's. 
  #it is notable, however, that the highest high-BMI outliers actually occur at about 21 and 38. 

ggplot(data, aes(x = as.numeric(RIDAGEYR), y = BMXBMI)) + #using ggplot to create the plot, specifying "data" as the input dataframe; specifying age as x and BMI as y; nesting as.numeric function to transform age into a numeric variable type 
  geom_point(shape = 21, color = "blue", fill = "white", size = 3) + #adding the points- specifying shape, color, fill, and size using geom_point() function
  labs(title = "Scatterplot of Age by BMI", x = "Age", y = "BMI") #using labs() function to add title, x and y axis labels 

ggsave("plot3.pdf", plot = plot3) #saving plot3 as pdf using ggsave() function 


#5.
#create a barplot of diet quality
#What does this graph show?

  #The graph shows that the highest prportion of participants rate their diet quality as "good" (about 38%). Next, similar proportions 
  #of participants rate their diet as "fair" or "very good". About 8% of participants think they have an excellent diet,
  #and only 5% of participants see their diet as poor. 

diet_quality <- data %>% #piping "data" dataframe through the count of diet quality (count() function)
  count(DBQ700) %>%      
  mutate(prop = n / sum(n)) #piping the count of diet quality through the mutate() function to create a new column representing the proportion of each level

plot4 <- ggplot(diet_quality, aes(x = DBQ700, y = prop, fill = DBQ700)) + #creating a plot object with new diet_quality dataframe using ggplot(); specifying input dataframe, x and y variables, and that diet quality will determine color levels represented in the graph
  geom_bar(stat = "identity") + #creating a bar 
  scale_fill_brewer(palette = "Set1") + #setting the color palette of the bars using scale_fill_brewer()
  labs(x = "Diet Quality", y = "Proportion", #labeling x, y, title using labs() function 
       title = "Proportion of Participant Ratings of their Diet Quality") +
  theme_minimal() #setting the theme to a minimal style 

ggsave("plot4.pdf", plot = plot4) #saving plot4 as pdf using ggsave() function 

#6 Using variables in the dataset, create a unique plot that tells the reader something about a relationship between at least two variables. Describe this relationship. 
#See the dataset description document for details about the variables in the dataset.

  #I decided to compare participant's mean BMI by education level, but with each education category broken down by participant's rating of their diet quality.
  #I chose these variables because I wanted to not only see the relationship between BMI and education level, but also how both of these factors may affect our perception of our diets.
  #I found surprising results. For one, every level of education has a nearly equal proportion of diet quality ratings across all the possible options, which 
  #tells me that education level is not a determinant of how we percieve the quality of our diet. 
  #no education-level group particularly preferred one quality rating over the other, and all possible quality ratings were present in each level of education. 
  #looking at BMI, there does seem to be a very loose exception-prone positive relationship between BMI and level of education.
  #those with some highschool have the highest BMI, while those with some college actually have a slightly higher BMI than  those who are highschool educated.
  #very surprisingly, those with no highschool actually have the second lowest BMI before those who are college-educated. 

# Calculate mean BMI by education level and diet quality
bmi_by_educ_dq <- data %>% #creating a new dataframe; piping the original dataframe into the next operation
  filter(!is.na(DMDEDUC2), !is.na(DBQ700)) %>% #removing NAs
  group_by(DMDEDUC2, DBQ700) %>% #grouping the data by education and diet using groupyby()
  summarize(mean_bmi = mean(BMXBMI, na.rm = TRUE))#calculating the mean of each group; saving as new column "mean_bmi" in the new dataframe; removing NAs before calculating 

# Create a stacked bar chart of mean BMI by education level and diet quality
plot_7 <- ggplot(bmi_by_educ_dq, aes(x = reorder(DMDEDUC2, -mean_bmi, FUN = mean), y = mean_bmi, fill = DBQ700)) + #creating a new plot object; specifies dataframe, x axis, y axis, and fill color mapping
  geom_bar(stat = "identity") + #adding the bars using geom.bar() function; stats = identity tells R to use the actual values in "mean_bmi" 
  scale_fill_brewer(palette = "Set1", limits = c("Poor", "Fair", "Good", "Very Good", "Excellent")) + #setting the color palette using scale_fill_brewer()
  labs(x = "Education Level", y = "Mean BMI", #setting x, y, title labels and fill label
       title = "Mean BMI by Education Level and Diet Quality",
       fill = "Diet Quality") +
  theme_minimal() #setting minimal theme using theme_minimal() function


ggsave("plot7.pdf", plot = plot_7) #saving plot_7 as pdf using ggsave() function 































