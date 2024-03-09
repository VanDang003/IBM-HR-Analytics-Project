# Load packages
library(dplyr)
library(ggplot2)


# Load Attrition Dataset
data = read.csv("HR_Attrition.csv", header = TRUE)
str(data)

# Change Attrition values to 1 for yes and 0 for no
data$Attrition = ifelse(data$Attrition == "Yes", 1, 0)
str(data)


# Run Binomial Regression with attrition as dependent and all other variables as independent
my_glm_1 = glm(Attrition ~ MaritalStatus  + Age + DailyRate + DistanceFromHome + EnvironmentSatisfaction  + RelationshipSatisfaction + StockOptionLevel + TotalWorkingYears + 
                   WorkLifeBalance + WorkLifeBalance + YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager, 
                 family = 'binomial', data = data)
summary(my_glm_1)

# Grouping Age
young <- data$Age < 25
middle_aged <- data$Age >= 25 & data$Age < 50
senior <- data$Age >= 50
data$Age_Group <- factor(
  ifelse(young, "Young",
         ifelse(middle_aged, "Middle Aged", "Senior")),
  levels = c("Young", "Middle Aged", "Senior")
)

# Regression my Age Group * Marital
my_glm_ageg = glm(Attrition ~ Age_Group*MaritalStatus , family = 'binomial', data = data)
summary(my_glm_ageg)

# Relevel Marital Status
data$MaritalStatus <- factor(data$MaritalStatus, ordered = FALSE)
data$MaritalStatus <- relevel(data$MaritalStatus, "Single")

# Regression on Marital Status
my_glm_test = glm(Attrition ~ MaritalStatus , family = 'binomial', data = data)
summary(my_glm_test)


# Others (Experimental)
my_glm_Personal = glm(Stay ~ Age + DistanceFromHome + MaritalStatus + NumCompaniesWorked + RelationshipSatisfaction, family = 'binomial', data = data2)
summary(my_glm_Personal)
