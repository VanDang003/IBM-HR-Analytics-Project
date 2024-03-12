# Load packages
library(dplyr)
library(ggplot2)
library(tidyverse)


# Load Attrition Dataset
getwd()
downloads_path <- file.path(Sys.getenv("HOME"), "Downloads")
setwd(downloads_path)
data_raw = read.csv("HR_Attrition.csv", header = TRUE)

head(data_raw)

### CLEANING DATA ###
# Remove EmployeeCount, Over18 & StandardHours as the data is similar across all rows. Thus, these variables are not useful.
data <- data_raw %>% select(-Over18) %>% select(-StandardHours) %>% select(-EmployeeCount)
# Change Attrition values to 1 for yes and 0 for no
data$Attrition = ifelse(data$Attrition == "Yes", 1, 0)


### CORRELATION MATRIX ###
correlation_matrix <- cor(data[, sapply(data, is.numeric)])
install.packages("caret")
library(caret)
high_corr <- findCorrelation(correlation_matrix, cutoff = 0.7)
print(names(data)[high_corr])

heatmap(correlation_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(100),
        symm = TRUE, margins = c(5, 5))


### USING VIF ### 
library(car)
my_glm = glm(Attrition ~ ., family = 'binomial', data = data)
vif_values <- vif(my_glm)
print(vif_values)

### USING PCA ###
## Changing non-numeric values to dummy variables
library(tidyverse)

# BusinessTravel column, with Travel_Frequently as base
dataPCA<- data %>%
  mutate(Travel_Rarely = ifelse(BusinessTravel=="Travel_Rarely",1,0)) %>%
  mutate('Non-Travel' = ifelse(BusinessTravel=="Non-Travel",1,0))

# Department column, with Sales as base
dataPCA<- dataPCA %>%
  mutate(R_D = ifelse(Department=="Research & Development",1,0)) %>%
  mutate(HR = ifelse(Department=="Human Resources",1,0))

# Education column, with Life Sciences as base
dataPCA<- dataPCA %>%
  mutate(Other_deg = ifelse(EducationField=="Life Sciences",1,0)) %>%
  mutate(Medical_deg = ifelse(EducationField=="Medical",1,0)) %>%
  mutate(Marketing_deg = ifelse(EducationField=="Marketing",1,0)) %>%
  mutate(Technical_deg = ifelse(EducationField=="Technical Degree",1,0)) %>%
  mutate(HR_deg = ifelse(EducationField=="Human Resources",1,0))

# Gender column, with Male as 1
dataPCA<- dataPCA %>%
  mutate(Male = ifelse(Gender=="Male",1,0))
  
#Job Role column, with Sales executive as base
dataPCA<- dataPCA %>%
  mutate(RS_job = ifelse(JobRole=="Research Scientist",1,0)) %>%
  mutate(LabTech_job = ifelse(JobRole=="Laboratory Technician",1,0)) %>%
  mutate(Manu_job = ifelse(JobRole=="Manufacturing Director",1,0)) %>%
  mutate(HC_job = ifelse(JobRole=="Healthcare Representative",1,0)) %>%
  mutate(Manager_job = ifelse(JobRole=="Manager",1,0)) %>%
  mutate(SalesRep_job = ifelse(JobRole=="Sales Representative",1,0)) %>%
  mutate(RDir_job = ifelse(JobRole=="Research Director",1,0)) %>%
  mutate(HR_job = ifelse(JobRole=="Human Resources",1,0))

# MaritalStatus column, with Single as base
dataPCA<- dataPCA %>%
  mutate(Married = ifelse(MaritalStatus=="Married",1,0)) %>%
  mutate(Divorced = ifelse(MaritalStatus=="Divorced",1,0))

# Overtime column
dataPCA<- dataPCA %>%
  mutate(OverTime = ifelse(OverTime=="Yes",1,0))
#remove relevant columns
dataPCA <- dataPCA %>% select(-BusinessTravel) %>% select(-Department) %>% select(-EducationField) %>% select(-Gender) %>% select(-JobRole) %>% select(-MaritalStatus)

head(dataPCA)

pca <- prcomp(dataPCA, scale. = TRUE)
pca_components <- pca$x
summary(pca)
loadings <- pca$rotation
print(loadings)
