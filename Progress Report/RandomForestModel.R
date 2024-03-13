#Initialize environment
rm(list=ls())

library(dplyr)
library(ggplot2)
library(car)
library(randomForest)
library(ggplot2)
library(tree)
library(caret)

#install.packages("rpart")
#install.packages("rpart.plot")
library("rpart")
library("rpart.plot")

#Load and clean data
data_raw = read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv", header=TRUE)

#Drop further unnecessary columns like employee number and count
bad <- c("EmployeeCount","EmployeeNumber", "Over18", "StandardHours") 
data = data_raw[,!(names(data_raw) %in% bad)] 

# Relevel Marital Status
data$MaritalStatus <- factor(data$MaritalStatus, ordered = FALSE)
data$MaritalStatus <- relevel(data$MaritalStatus, "Single")

# Change Attrition values to 1 for yes and 0 for no
data$Attrition = ifelse(data$Attrition == "Yes", 1, 0)
data$Attrition <- as.factor(data$Attrition)

#------------------------------------------

#RandomForest with all predictors - For testing and overview purposes

#Create model, print data, and plot
#Number of predictors to use: 1+log(24) ~= 4.17 predictors
model_tree_whole <- randomForest(Attrition~., data = data, mtry = 4, importance = TRUE)
model_tree_whole
model_tree_whole$importance

varImpPlot(model_tree_whole)

#------------------------------------------
#RandomForest with our selected predictors

#Create model, print data, and plot
#Number of predictors to use: 1+log(12) ~= 3.5 predictors
model_tree <- randomForest(Attrition ~ Age + BusinessTravel + DistanceFromHome
+ EnvironmentSatisfaction + JobInvolvement + JobSatisfaction + WorkLifeBalance
+ MaritalStatus + OverTime + NumCompaniesWorked + RelationshipSatisfaction
+ TotalWorkingYears, data = data, mtry = 4, importance = TRUE)

model_tree
model_tree$importance
varImpPlot(model_tree)

#fit model with rpart for plotting
model_tree2 <- rpart(Attrition ~ Age + BusinessTravel + DistanceFromHome
                      + EnvironmentSatisfaction + JobInvolvement + JobSatisfaction + WorkLifeBalance
                      + MaritalStatus + OverTime + NumCompaniesWorked + RelationshipSatisfaction
                      + TotalWorkingYears, data = data)

# plot the tree
rpart.plot(model_tree2)

#------------------------------------------
#RandomForest with a composite life score

#Create a composite score and drop original columns from the data
data1 <- data
data1$composite_score <- data$WorkLifeBalance + data$EnvironmentSatisfaction + data$RelationshipSatisfaction + data$JobInvolvement
composite_names <- c("WorkLifeBalance","EnvironmentSatisfaction", "RelationshipSatisfaction") 
data1 = data1[,!(names(data1) %in% composite_names)] 

#Create model, print data, and plot
#Number of predictors to use: 1+log(9) ~= 3.19 predictors
model_tree_composite <- randomForest(Attrition ~ Age + BusinessTravel + DistanceFromHome
                   + composite_score
                   + MaritalStatus + OverTime + NumCompaniesWorked
                   + TotalWorkingYears + NumCompaniesWorked, data = data1, mtry = 3, importance = TRUE)

model_tree_composite
model_tree_composite$importance
varImpPlot(model_tree_composite)

#fit model with rpart for plotting
model_tree_composite2 <- rpart(Attrition ~ Age + BusinessTravel + DistanceFromHome
                               + composite_score
                               + MaritalStatus + OverTime + NumCompaniesWorked
                               + TotalWorkingYears + NumCompaniesWorked, data = data1)

# plot the tree
rpart.plot(model_tree_composite2)

#------------------------------------------
#RandomForest with Loyalty Score

#Create a loyalty score and drop original columns from the data
data2 <- data
data2$TotalWorkingYears = ifelse(data2$TotalWorkingYears == 0, 1, data2$TotalWorkingYears) #Eliminate zero values
data2$loyalty_score <- data2$NumCompaniesWorked/data2$TotalWorkingYears
loyalty_names <- c("NumCompaniesWorked","TotalWorkingYears") 
data2 = data2[,!(names(data2) %in% loyalty_names)]

#Create model, print data, and plot
#Number of predictors to use: 1+log(11) ~= 3.39 predictors
model_tree_loyalty <- randomForest(Attrition ~ Age + BusinessTravel + DistanceFromHome
                           + EnvironmentSatisfaction + JobInvolvement + JobSatisfaction + WorkLifeBalance
                           + MaritalStatus + OverTime + loyalty_score + RelationshipSatisfaction
                           , data = data2, mtry = 4, importance = TRUE)

model_tree_loyalty
model_tree_loyalty$importance
varImpPlot(model_tree_loyalty)

#fit model with rpart for plotting
model_tree_loyalty2 <- rpart(Attrition ~ Age + BusinessTravel + DistanceFromHome
                             + EnvironmentSatisfaction + JobInvolvement + JobSatisfaction + WorkLifeBalance
                             + MaritalStatus + OverTime + loyalty_score + RelationshipSatisfaction
                             , data = data2)

# plot the tree
rpart.plot(model_tree_loyalty2)

#------------------------------------------
#Regression Tree with Loyalty Score and Composite Score
data3 <- data2
data3$composite_score <- data$WorkLifeBalance + data$EnvironmentSatisfaction + data$RelationshipSatisfaction + data$JobInvolvement
data3 = data3[,!(names(data3) %in% composite_names)]


model_tree_tot <- tree(Attrition ~ Age + BusinessTravel + DistanceFromHome
                           + MaritalStatus + OverTime + loyalty_score + composite_score
                           , data = data3)
summary(model_tree_tot)

model_tree_tot$frame
summary(model_tree_tot)
plot(model_tree_tot)
text(model_tree_tot)
title("Attrition Tree - Loyalty scores")