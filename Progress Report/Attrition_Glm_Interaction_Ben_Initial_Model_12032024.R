# Load packages
library(dplyr)
library(ggplot2)
library(car)


# Load Attrition Dataset
data_raw = read.csv("HR_Attrition.csv", header = TRUE)
head(data_raw)

# Remove Over18, StandardHours, EmployeeCount & EmployeeNumber  as the data is similar across all rows. Thus, these variables are not useful.
data <- data_raw %>% select(-Over18) %>% select(-StandardHours) %>%
  select(-EmployeeCount) %>% select(-EmployeeNumber)

# Change Attrition values to 1 for yes and 0 for no
data$Attrition = ifelse(data$Attrition == "Yes", 1, 0)
str(data)

# Run Binomial Regression with attrition as dependent and all other variables as independent
my_glm = glm(Attrition ~ ., family = 'binomial', data = data)
summary(my_glm)
vif(my_glm)

# Relevel Marital Status
data$MaritalStatus <- factor(data$MaritalStatus, ordered = FALSE)
data$MaritalStatus <- relevel(data$MaritalStatus, "Single")

# Grouping Age
young_age <- data$Age < 26
middle_age <- data$Age >= 26 & data$Age < 36
mature_age <- data$Age >= 36 & data$Age < 46
senior_age <- data$Age >= 46
data$Age_Group <- factor(
  ifelse(young_age, "Young Age",
         ifelse(middle_age, "Middle Age",
                ifelse(mature_age, "Mature Age", "Senior Age"))),
  levels = c("Young Age", "Middle Age", "Mature Age", "Senior Age")
)

# Removed highly not significant independent variables
my_glm_1 = glm(Attrition ~ Age_Group + BusinessTravel + DistanceFromHome + 
                 EnvironmentSatisfaction + JobInvolvement + JobSatisfaction +
                 MaritalStatus + OverTime + RelationshipSatisfaction +
                 TrainingTimesLastYear + WorkLifeBalance +
                 NumCompaniesWorked + TotalWorkingYears , 
                 family = 'binomial', data = data)
summary(my_glm_1)
vif(my_glm_1)


# Creating new independent variables by combining significant variables with high VIF

data_2 <- data %>% mutate(loyalty = data$TotalWorkingYears / (data$NumCompaniesWorked +1))
data_2

# Experimental Model 2
my_glm_2 = glm(Attrition ~ Age_Group + BusinessTravel + DistanceFromHome + 
                 EnvironmentSatisfaction + JobInvolvement + JobSatisfaction +
                 MaritalStatus + OverTime + RelationshipSatisfaction +
                 TrainingTimesLastYear + WorkLifeBalance +
                 loyalty , 
               family = 'binomial', data = data_2)
summary(my_glm_2)
vif(my_glm_2)
