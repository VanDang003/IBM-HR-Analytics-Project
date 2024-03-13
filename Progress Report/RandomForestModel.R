#Initialize environment
rm(list=ls())

library(dplyr)
library(ggplot2)
library(car)
library(randomForest)
library(ggplot2)
library(tree)
library(caret)

#Load and clean data
data_raw = read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv", header=TRUE)

#Drop further unnecessary columns like employee number and count
bad <- c("EmployeeCount","EmployeeNumber", "Over18", "StandardHours") 

data = data_raw[,!(names(data_raw) %in% bad)] 

as.factor(data$OverTime)
as.factor(data$MaritalStatus)
# Relevel Marital Status
data$MaritalStatus <- factor(data$MaritalStatus, ordered = FALSE)
data$MaritalStatus <- relevel(data$MaritalStatus, "Single")

# Change Attrition values to 1 for yes and 0 for no
data$Attrition = ifelse(data$Attrition == "Yes", 1, 0)


#------------------------------------------

#RandomForest with all predictors
model_tree_whole <- tree(Attrition~., data = data)
model_tree_whole$frame
summary(model_tree_whole)
plot(model_tree_whole)
text(model_tree_whole)
title("Attrition Tree - All Predictors")

for (i in 2:7) {
  prune_tree_whole <- prune.tree(model_tree_whole, best = i)
  yhat2 <- predict(prune_tree_whole)
  SSres <- sum((yhat2 - data$Attrition)^2)
  SStot <- sum((data$Attrition - mean(data$Attrition))^2)
  r2 <- 1 - (SSres/SStot)
  print(sprintf("Branches: %f R2: %f", i, r2))
}

#------------------------------------------

#RandomForest with our selected predictors
model_tree <- tree(Attrition ~ Age + BusinessTravel + DistanceFromHome
+ EnvironmentSatisfaction + JobInvolvement + JobSatisfaction + WorkLifeBalance
+ MaritalStatus + OverTime + NumCompaniesWorked + RelationshipSatisfaction
+ TotalWorkingYears + NumCompaniesWorked, data = data)
summary(model_tree)

model_tree$frame
summary(model_tree)
plot(model_tree)
text(model_tree)
title("Attrition Tree - Selected Predictors")

for (i in 2:7) {
  prune_tree <- prune.tree(model_tree, best = i)
  yhat2 <- predict(prune_tree)
  SSres <- sum((yhat2 - data$Attrition)^2)
  SStot <- sum((data$Attrition - mean(data$Attrition))^2)
  r2 <- 1 - (SSres/SStot)
  print(sprintf("Branches: %f R2: %f", i, r2))
}

#------------------------------------------
data1 <- data
data1$composite_score <- data$WorkLifeBalance + data$EnvironmentSatisfaction + data$RelationshipSatisfaction + data$JobInvolvement
composite_names <- c("WorkLifeBalance","EnvironmentSatisfaction", "RelationshipSatisfaction") 
data1 = data1[,!(names(data1) %in% composite_names)] 

model_tree_composite <- tree(Attrition ~ Age + BusinessTravel + DistanceFromHome
                   + composite_score
                   + MaritalStatus + OverTime + NumCompaniesWorked
                   + TotalWorkingYears + NumCompaniesWorked, data = data1)
summary(model_tree_composite)

model_tree_composite$frame
summary(model_tree_composite)
plot(model_tree_composite)
text(model_tree_composite)
title("Attrition Tree - Composite scores")

for (i in 2:7) {
  prune_tree_composite <- prune.tree(model_tree_composite, best = i)
  yhat2 <- predict(prune_tree_composite)
  SSres <- sum((yhat2 - data$Attrition)^2)
  SStot <- sum((data$Attrition - mean(data$Attrition))^2)
  r2 <- 1 - (SSres/SStot)
  print(sprintf("Branches: %f R2: %f", i, r2))
}