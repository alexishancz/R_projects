#load packages needed
library(package = "tidyverse")
library(package = "janitor")
library(package = "dplyr")
library(package = "descr")
library(package = "car")
library(package = "odds.n.ends")

#read the data
library.data <- read.csv("QMSS Group Project Final Version.csv", header = T)

#summary of the data
summary(library.data)

#clean the data
library.data$libusea.clean <- as.factor(library.data$libusea.clean)
library.data$libuseb.clean <- as.factor(library.data$libuseb.clean)

library.data$Gender <- recode(library.data$sex, "1 = 'Male' ; 2 = 'Female'")
library.data$Gender <- as.factor(library.data$Gender)

library.data$ParentalStatus <- recode(library.data$parental.status, "1 = 'Yes' ; 2 = 'No'")
library.data$ParentalStatus <- as.factor(library.data$ParentalStatus)

library.data$Income <- as.factor(library.data$inc.cleaned)
library.data$Income <- na_if(x = library.data$Income, y = "")

library.data$Rurality <- recode(library.data$rurality.community.type, "1 = 'A Large City' ; 2 = 'A Suburb Near A Large City' ; 3 = 'A Small City or Town, OR'; 4 = 'A Rural Area'")
library.data$Rurality <- as.factor(library.data$Rurality)

library.data$NumbersofBooksRead <- as.numeric(library.data$books.read.in.last.12.months)

#Run t-test comparing library usages(libusea) by age and library usages(libuseb) by age
TTESTLIBAAGE <- t.test(formula = library.data$age ~ library.data$libusea.clean, na.rm=T)
TTESTLIBAAGE
TTESTLIBBAGE <- t.test(formula = library.data$age ~ library.data$libuseb.clean)
TTESTLIBBAGE

#Run bivariate table and chi-squared test for library usages(libusea) by gender and library usages(libuseb) by gender
table(library.data$libusea.clean, library.data$Gender)
chisq.test(x = library.data$libusea.clean,
           y = library.data$Gender)

table(library.data$libuseb.clean, library.data$Gender)
chisq.test(x = library.data$libuseb.clean,
           y = library.data$Gender)

#Run Crosstabs with %'s and Sums
#Load function that creates Column percentages with crosstab instruction
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
crosstab(library.data, row.vars = "libusea.clean", col.vars = "Gender", type = "c")
crosstab(library.data, row.vars = "libuseb.clean", col.vars = "Gender", type = "c")

#Run bivariate table and chi-squared test for library usages(libusea) by parental status and library usages(libuseb) by parental status
table(library.data$libusea.clean, library.data$ParentalStatus)
chisq.test(x = library.data$libusea.clean,
           y = library.data$ParentalStatus)

table(library.data$libuseb.clean, library.data$ParentalStatus)
chisq.test(x = library.data$libuseb.clean,
           y = library.data$ParentalStatus)

#Run Crosstabs with %'s and Sums
#Load function that creates Column percentages with crosstab instruction
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
crosstab(library.data, row.vars = "libusea.clean", col.vars = "ParentalStatus", type = "c")
crosstab(library.data, row.vars = "libuseb.clean", col.vars = "ParentalStatus", type = "c")

#Run t-test comparing library usages(libusea) by income and library usages(libuseb) by income
TTESTLIBAI <- t.test(formula = library.data$libusea.clean ~ library.data$Income)
TTESTLIBAI
TTESTLIBBI <- t.test(formula = library.data$libuseb.clean ~ library.data$Income)
TTESTLIBBI


#Compute the analysis of variance looking at library usages (libusea) for each of the 4 income conditions
INCOMELIBUSEAaov <- aov(libusea.clean ~ Income, data = library.data)
# Summary of the analysis
summary(ConditionATTaov)

#Compute the analysis of variance looking at library usages (libusea) for each of the 4 rurality conditions
RURALITYLIBUSEAaov <- aov(libusea.clean ~ Rurality, data = library.data)
# Summary of the analysis
summary(ConditionATTaov)

#Run t-test comparing library usages(libusea) by number of books read and library usages(libuseb)
TTESTLIBANB <- t.test(formula = library.data$NumbersofBooksRead ~ library.data$libusea.clean)
TTESTLIBANB
TTESTLIBBNB <- t.test(formula = library.data$NumbersofBooksRead ~ library.data$libuseb.clean)
TTESTLIBBNB

#glm() for binomial logistic regression (dichotomous dependent variable)
glma <- glm(library.data$libusea.clean ~ library.data$age + library.data$Income + library.data$Rurality + library.data$NumbersofBooksRead, family = binomial("logit"))
glmb <- glm(library.data$libuseb.clean ~ library.data$age + library.data$Income + library.data$Rurality + library.data$NumbersofBooksRead, family = binomial("logit"))

#summary of glm
summary(glma)
summary(glmb)

## run odds.n.ends on your glm object to get odds ratios of coefficients
odds.n.ends(glma)
odds.n.ends(g)


