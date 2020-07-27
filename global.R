### Loading packages that we will be using

#devtools::install_github("vqv/ggbiplot")
#devtools::install_github("bwlewis/rthreejs")
#devtools::install_github("ramnathv/rCharts")
library(ggbiplot)
#library(rthreejs)
library(rCharts)



library(RCurl)
library(knitr)
library(plyr)
library(ggbiplot)
library(rCharts)
library(qcc)
library(threejs)
library(rgl)
library(pca3d)
library(gridExtra)

link2 = './IBM_Attrition.csv'

# All column names in the dataset
IBMnames = c(
  "EmployeeNumber", "Department",
  "Attrition", "AttritInt", "color",
  
  #quantitative fields
  "Age", "DistanceFromHome", "HourlyRate", "DailyRate", 
  "MonthlyRate", "MonthlyIncome", "PercentSalaryHike", 
  "TrainingTimesLastYear", "NumCompaniesWorked", "TotalWorkingYears", 
  "YearsAtCompany", "YearsInCurrentRole", "YearsSinceLastPromotion", 
  "YearsWithCurrManager",
  
  #qualitative fields
  "Education", "EnvironmentSatisfaction", "JobInvolvement", 
  "JobLevel", "JobSatisfaction", "PerformanceRating", 
  "RelationshipSatisfaction", "StockOptionLevel", "WorkLifeBalance",
  
  #nominal and binary data
  "EducationField", "BusinessTravel", "Gender", "JobRole", 
  "MaritalStatus", "OverTime", "Over18")

# Quantitative data fields only
IBMQuant = c(
  #"EmployeeNumber",
  #"Attrition", "AttritInt",
  
  #quantitative fields
  "Age", "DistanceFromHome", "HourlyRate", "DailyRate", 
  "MonthlyRate", "MonthlyIncome", "PercentSalaryHike", 
  "TrainingTimesLastYear", "NumCompaniesWorked", "TotalWorkingYears", 
  "YearsAtCompany", "YearsInCurrentRole", "YearsSinceLastPromotion", 
  "YearsWithCurrManager",
  
  #qualitative fields
  "Education", "EnvironmentSatisfaction", "JobInvolvement", 
  "JobLevel", "JobSatisfaction", "PerformanceRating", 
  "RelationshipSatisfaction", "StockOptionLevel", "WorkLifeBalance"
  )

IBMDef = c(
  #quantitative fields
  "Age", "DistanceFromHome",
  "MonthlyIncome", "PercentSalaryHike", 
  "TrainingTimesLastYear", "NumCompaniesWorked", "TotalWorkingYears", 
  "YearsAtCompany", "YearsInCurrentRole", "YearsSinceLastPromotion", 
  "YearsWithCurrManager",
  
  #qualitative fields
  "Education", "EnvironmentSatisfaction", "JobInvolvement", 
  "JobLevel", "JobSatisfaction", "PerformanceRating", 
  "RelationshipSatisfaction", "StockOptionLevel", "WorkLifeBalance"
)

QuantLen = length(IBMQuant)
dataIBM = read.csv(link2,header=T)


### Clean up the data for our analysis
names(dataIBM)[1] = "Age"
dataIBM = mutate(dataIBM, color=ifelse(dataIBM$Attrition=="Yes",'red','green'))
dataIBM = mutate(dataIBM, AttritInt=ifelse(Attrition=="Yes",1,0))
dataIBM$Attrition = as.factor(dataIBM$Attrition)
dataIBM = dataIBM[, IBMnames]


### Mean centering data
#
#head(dataIBM[,6:QuantLen])
IbmMC = apply(dataIBM[,6:QuantLen], 2, function(y) y - mean(y))

# 
### Apply PCA
pca = prcomp(dataIBM[,IBMQuant], center=T, scale.=T)


#Holderover code from course example, useful for debugging 

### Investigate PCA
# str(pca)

### Eigen Values
#round(pca$sdev^2, 2)
# 

### Extract PCs
pcs = data.frame(pca$x)
# str(pcs)
# 

### Percentage of variance explained by each PC
cov = round(pca$sdev^2/sum(pca$sdev^2)*100, 2)
cov = data.frame(c(1:QuantLen),cov)
names(cov)[1] = 'PCs'
names(cov)[2] = 'Variance'
#cov
 

