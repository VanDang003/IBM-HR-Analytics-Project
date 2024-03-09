# Load packages
library(dplyr)
library(ggplot2)


# Load Attrition Dataset
data_raw = read.csv("HR_Attrition.csv", header = TRUE)
str(data)

# Remove Over18 & StandardHours as the data is similar across all rows. Thus, these variables are not useful.
data <- data_raw %>% select(-Over18) %>% select(-StandardHours)

# Change Attrition values to 1 for yes and 0 for no
data$Attrition = ifelse(data$Attrition == "Yes", 1, 0)
str(data)

# Run Binomial Regression with attrition as dependent and all other variables as independent
my_glm = glm(Attrition ~ ., family = 'binomial', data = data)
summary(my_glm)

# Removed highly not significant independent variables
my_glm_1 = glm(Attrition ~ Age + BusinessTravel + DistanceFromHome + 
                 EnvironmentSatisfaction  + Gender + JobInvolvement + JobSatisfaction +
                 MaritalStatus + NumCompaniesWorked + OverTime + RelationshipSatisfaction +
                 TotalWorkingYears + TrainingTimesLastYear +
                 WorkLifeBalance + YearsAtCompany + YearsInCurrentRole +
                 YearsSinceLastPromotion + YearsWithCurrManager, 
                 family = 'binomial', data = data)
summary(my_glm_1)

# Relevel Marital Status
data$MaritalStatus <- factor(data$MaritalStatus, ordered = FALSE)
data$MaritalStatus <- relevel(data$MaritalStatus, "Single")

# Grouping Age
young <- data$Age < 25
middle_aged <- data$Age >= 25 & data$Age < 50
senior <- data$Age >= 50
data$Age_Group <- factor(
  ifelse(young, "Young",
         ifelse(middle_aged, "Middle Aged", "Senior")),
  levels = c("Young", "Middle Aged", "Senior")
)

# Regression on Marital Status
my_glm_test = glm(Attrition ~ MaritalStatus , family = 'binomial', data = data)
summary(my_glm_test)

# Glm Regression with Age Group * MaritalStatus as interaction term
my_glm_ageg = glm(Attrition ~ Age_Group*MaritalStatus , family = 'binomial', data = data)
summary(my_glm_ageg)

# Experimental
my_glm_Personal = glm(Stay ~ Age + DistanceFromHome + MaritalStatus + NumCompaniesWorked + RelationshipSatisfaction, family = 'binomial', data = data2)
summary(my_glm_Personal)
my_glm_other = glm(Attrition ~ MaritalStatus  + Age_Group + DistanceFromHome + EnvironmentSatisfaction + WorkLifeBalance + RelationshipSatisfaction + StockOptionLevel + TotalWorkingYears + 
                  + YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager, 
               family = 'binomial', data = data)
summary(my_glm_other)


# Working Code
