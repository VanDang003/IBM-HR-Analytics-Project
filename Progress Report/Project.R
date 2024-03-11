rm(list=ls())

library(dplyr)
library(ggplot2)
library(car)
library(corrplot)

data_raw = read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv", header=TRUE)

#Drop further unnecessary columns like employee number and count
bad <- c("EmployeeCount","EmployeeNumber", "Over18", "StandardHours") 
data = data_raw[,!(names(data_raw) %in% bad)] 

# Relevel Marital Status
data$MaritalStatus <- factor(data$MaritalStatus, ordered = FALSE)
data$MaritalStatus <- relevel(data$MaritalStatus, "Single")

# Change Attrition values to 1 for yes and 0 for no
data$Attrition = ifelse(data$Attrition == "Yes", 1, 0)

#Full model
my_glm = glm(Attrition ~ ., family = 'binomial', data = data)
summary(my_glm)

#overall regression statistically significant
1-pchisq((my_glm$null.dev - my_glm$deviance), (my_glm$df.null - my_glm$df.resid))
# significant

#goodness-of-fit of model1 using both deviance residuals
c(deviance(my_glm), 1-pchisq(deviance(my_glm),my_glm$df.resid))
#Model is a good fit




# Removed highly not significant independent variables
my_glm_1 = glm(Attrition ~ Age + BusinessTravel + DistanceFromHome + 
                 EnvironmentSatisfaction  + Gender + JobInvolvement + JobSatisfaction +
                 MaritalStatus + NumCompaniesWorked + OverTime + RelationshipSatisfaction +
                 TotalWorkingYears + TrainingTimesLastYear +
                 WorkLifeBalance + YearsAtCompany + YearsInCurrentRole +
                 YearsSinceLastPromotion + YearsWithCurrManager, 
               family = 'binomial', data = data)
summary(my_glm_1)

vif_values <- vif(my_glm_1)
vif_values

#overall regression statistically significant
1-pchisq((my_glm_1$null.dev - my_glm_1$deviance), (my_glm_1$df.null - my_glm_1$df.resid))
# significant

#goodness-of-fit of model1 using deviance residuals
c(deviance(my_glm), 1-pchisq(deviance(my_glm_1),my_glm_1$df.resid))
#Model is a good fit



# Grouping Age
young <- data$Age < 25
middle_aged <- data$Age >= 25 & data$Age < 50
senior <- data$Age >= 50
data$Age_Group <- factor(
  ifelse(young, "Young",
         ifelse(middle_aged, "Middle Aged", "Senior")),
  levels = c("Young", "Middle Aged", "Senior")
)



#Corplot needs data to be numerical so data needs to be changed to dummy variables first.
#This data is only partially converted
data <- data %>%
  mutate(Travel_Rarely = ifelse(BusinessTravel=='Travel_Rarely',1,0)) %>%
  mutate(Travel_Frequently = ifelse(BusinessTravel=='Travel_Frequently', 1, 0))  %>%
  mutate(Sales = ifelse(Department =='Sales', 1, 0))  %>%
  mutate(RnD = ifelse(Department=='Research & Development', 1, 0))  %>%
  mutate(EduLifeSciences = ifelse(EducationField=='Life Sciences', 1, 0))  %>%
  mutate(EduMedical = ifelse(EducationField=='Medical', 1, 0))  %>%
  mutate(EduMarketing = ifelse(EducationField=='Marketing', 1, 0))  %>%
  mutate(EduTechnical = ifelse(EducationField=='Technical Degree', 1, 0))  %>%
  mutate(EduHR = ifelse(EducationField=='Human Resources', 1, 0))  %>%
  mutate(EduOther = ifelse(EducationField=='Other', 1, 0))  %>%
  mutate(JobHR = ifelse(JobRole=='Human Resources', 1, 0))  %>%
  mutate(JobLabTech = ifelse(JobRole=='Laboratory Technician', 1, 0))  %>%
  mutate(JobManufacturerDirector = ifelse(JobRole=='Manufacturing Director', 1, 0))  %>%
  mutate(JobResearchDirector = ifelse(JobRole=='Research Director', 1, 0))  %>%
  mutate(JobManager = ifelse(JobRole=='Manager', 1, 0))  %>%
  mutate(JobResearchScientist = ifelse(JobRole=='Research Scientist', 1, 0))  %>%
  mutate(JobRoleSalesExecutive = ifelse(JobRole=='RoleSales Executive', 1, 0))  %>%
  mutate(JobSalesRepresentative = ifelse(JobRole=='Sales Representative', 1, 0))  %>%
  mutate(Divorced = ifelse(MaritalStatus=='Divorced', 1, 0))  %>%
  mutate(Married = ifelse(MaritalStatus=='Married ', 1, 0))  %>%
  mutate(OverTime = ifelse(OverTime=='Yes', 1, 0))
  
subset_data <- data[c("Attrition", "Age", "Travel_Rarely", "Travel_Frequently", "Sales", "RnD", "EduLifeSciences",
                      "EduMedical", "EduMarketing", "EduTechnical", "EduHR", "EduOther",
                      "JobHR", "JobLabTech", "JobManufacturerDirector", 
                      "JobResearchDirector", "JobManager", "JobResearchScientist", 
                      "JobRoleSalesExecutive", "JobSalesRepresentative",
                      "Divorced", "Married", "OverTime", "DailyRate", "DistanceFromHome",
                      "Education", "EnvironmentSatisfaction", "HourlyRate",
                      "JobInvolvement",	"JobLevel", "JobSatisfaction", "MonthlyIncome",
                      "MonthlyRate",	"NumCompaniesWorked", "PercentSalaryHike",
                      "PerformanceRating",	"RelationshipSatisfaction", 
                      "StockOptionLevel",	"TotalWorkingYears",	"TrainingTimesLastYear",
                      "WorkLifeBalance",	"YearsAtCompany",	"YearsInCurrentRole",
                      "YearsSinceLastPromotion",	"YearsWithCurrManager")]

cor_matrix<-cor(subset_data)
corrplot(cor_matrix, method="number")

top_cor_indices <- which(upper.tri(cor_matrix, diag = FALSE), arr.ind = TRUE)
top_cor_values <- cor_matrix[top_cor_indices]

# Get the top 10 correlations and corresponding variable pairs
top_10_correlations <- data.frame(
  Variable1 = rownames(cor_matrix)[top_cor_indices[, 1]],
  Variable2 = colnames(cor_matrix)[top_cor_indices[, 2]],
  Correlation = top_cor_values
) %>%
  arrange(desc(abs(Correlation))) %>%
  head(10)

# Print the top 10 correlations
print(top_10_correlations)





test_glm = glm(Attrition ~ Age +Travel_Rarely+Travel_Frequently+Sales+RnD+EduLifeSciences+
               EduMedical+EduMarketing+EduTechnical+EduHR+EduOther+JobHR+JobLabTech+JobManufacturerDirector 
               +JobResearchDirector+JobManager+JobResearchScientist+ 
               JobRoleSalesExecutive+JobSalesRepresentative+
               Divorced+Married+OverTime+DailyRate+DistanceFromHome+
               Education+EnvironmentSatisfaction+HourlyRate+
               JobInvolvement+JobLevel+JobSatisfaction+MonthlyIncome+
               MonthlyRate+NumCompaniesWorked+PercentSalaryHike+
               PerformanceRating+RelationshipSatisfaction+
               StockOptionLevel+TotalWorkingYears+TrainingTimesLastYear+
               WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+
               YearsSinceLastPromotion+YearsWithCurrManager,
               family = 'binomial', data = data)
summary(test_glm)

