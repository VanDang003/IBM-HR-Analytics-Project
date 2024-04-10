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
data$Attrition <- as.factor(data$Attrition)
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

# Perform k-fold cross-validation
library(caret)
ctrl <- trainControl(method = "cv", number = 10)
cv_glm_1 <- train(Attrition ~ Age_Group + BusinessTravel + DistanceFromHome + 
                    EnvironmentSatisfaction + JobInvolvement + JobSatisfaction +
                    MaritalStatus + OverTime + RelationshipSatisfaction +
                    TrainingTimesLastYear + WorkLifeBalance +
                    NumCompaniesWorked + TotalWorkingYears, data = data, method = "glm", family = "binomial", 
             trControl = ctrl)
print(cv_glm_1)
summary(cv_glm_1)


# Creating new independent variables by combining significant variables with high VIF

data_2 <- data %>% mutate(loyalty = data$TotalWorkingYears / (data$NumCompaniesWorked +1))
data_2

# Model 2 with loyalty variable
my_glm_2 = glm(Attrition ~ Age_Group + BusinessTravel + DistanceFromHome + 
                 EnvironmentSatisfaction + JobInvolvement + JobSatisfaction +
                 MaritalStatus + OverTime + RelationshipSatisfaction +
                 TrainingTimesLastYear + WorkLifeBalance +
                 loyalty , 
               family = 'binomial', data = data_2)
summary(my_glm_2)
vif(my_glm_2)

# Split ratio (e.g., 70% for training)
split_ratio <- 0.7

training_data <- data_2 %>% 
  sample_frac(split_ratio, seed = 123)

testing_data <- data_2 %>% 
  filter(!rownames(.) %in% rownames(training_data))

# Perform k-fold cross-validation
library(caret)
ctrl <- trainControl(method = "cv", number = 10)
cv_glm_2 <- train(Attrition ~ Age_Group + BusinessTravel + DistanceFromHome + 
                    EnvironmentSatisfaction + JobInvolvement + JobSatisfaction +
                    MaritalStatus + OverTime + RelationshipSatisfaction +
                    TrainingTimesLastYear + WorkLifeBalance +
                    loyalty, data = training_data, method = "glm", family = "binomial", 
                  trControl = ctrl)
print(cv_glm_2)
summary(cv_glm_2)
varImp(cv_glm_2)

# Training Model (removed Training, RelationshipSatisfaction, WorkLifeBalance)

my_glm_train = glm(Attrition ~ Age_Group + BusinessTravel + DistanceFromHome + 
                     EnvironmentSatisfaction + JobInvolvement + JobSatisfaction +
                     MaritalStatus + OverTime +
                    
                     loyalty , 
                   family = 'binomial', data = training_data)
summary(my_glm_train)

# Predict probability for each customer
probabilities <- predict(my_glm_train, testing_data, type = "response")
head(probabilities, 5)
probabilities

# Create different cutoffs and calculate accuracy
cutoffs <- c(0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8)

accuracies <- sapply(cutoffs, function(cutoff) {
  # Convert probabilities to binary predictions
  predictions <- ifelse(probabilities > cutoff, 1, 0)
  
  # Calculate accuracy
  mean(predictions == testing_data$Attrition)
})

# Print the accuracies for each cutoff
names(accuracies) <- cutoffs
round(accuracies, 3)

# Cutoff with highest accuracy
optimal_cutoff <- names(which.max(accuracies))
optimal_cutoff

# Generate predictions using the optimal cutoff
optimal_predictions <- ifelse(probabilities > as.numeric(optimal_cutoff), 1, 0)

# Convert both predictions and actual values to factor
optimal_predictions_factor <- factor(optimal_predictions, levels = c(1, 0))
actual_values_factor <- factor(testing_data$Attrition, levels = c(1, 0))

# Create confusion matrix
library(caret)
conf_matrix <- confusionMatrix(optimal_predictions_factor, actual_values_factor, positive = '1')

# Print the confusion matrix
print(conf_matrix)