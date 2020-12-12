#
# Load libraries and define working directory
#
setwd("C:/Users/brwilc/Dropbox/Teaching/USC ISE-535 Data Mining - Fall 2020/Final Project")
library(tidyverse)
#
#  Define summarize_numeric function from homework 2 
#
summarize_numeric = function(dataset) {
  
  dataset = select_if(dataset, is.numeric)
  summary.table = data.frame(Attribute = names(dataset))
  
  summary.table = summary.table %>% 
    mutate('Missing Values' = apply(dataset, 2, function (x) sum(is.na(x))),
           'Unique Values' = apply(dataset, 2, function (x) length(unique(x))),
           'Mean' = colMeans(dataset, na.rm = TRUE),
           'Min' = apply(dataset, 2, function (x) min(x, na.rm = TRUE)),
           'Max' = apply(dataset, 2, function (x) max(x, na.rm = TRUE)),
           'SD' = apply(dataset, 2, function (x) sd(x, na.rm = TRUE))
    )
  summary.table
}
#
#  Define summarize_character function 
#
summarize_character = function(dataset) {
  
  dataset = select_if(dataset, is.character)
  summary.table = data.frame(Attribute = names(dataset))
  
  summary.table = summary.table %>% 
    mutate('Missing Values' = apply(dataset, 2, function (x) sum(is.na(x))),
           'Unique Values' = apply(dataset, 2, function (x) length(unique(x))),
    )
  summary.table
}
#
#  Initial dataset summary and conversion of attributes to appropriate types (numerics and classes)
#
employee_attrition = read_csv('Employee_attrition.csv')
summarize_numeric(employee_attrition)
summarize_character(employee_attrition)
#
#  Convert numeric attributes to factors
#
employee_attrition = employee_attrition %>% mutate(Education = as_factor(
  case_when(
    Education == 1 ~ 'Below College',
    Education == 2 ~ 'College',
    Education == 3 ~ 'Bachelor',
    Education == 4 ~ 'Master',
    Education == 5 ~ 'Doctor')
  )
)
employee_attrition$Education = factor(employee_attrition$Education, order = TRUE, 
                                      levels = c("Below College", "College", "Bachelor", "Master", "Doctor"))

employee_attrition = employee_attrition %>% mutate(EnvironmentSatisfaction = as.factor(
  case_when(
    EnvironmentSatisfaction == 1 ~ 'Low',
    EnvironmentSatisfaction == 2 ~ 'Medium',
    EnvironmentSatisfaction == 3 ~ 'High',
    EnvironmentSatisfaction == 4 ~ 'Very High')
)
)

employee_attrition$EnvironmentSatisfaction = factor(employee_attrition$EnvironmentSatisfaction, order = TRUE, 
                                      levels = c("Low", "Medium", "High", "Very High"))

employee_attrition = employee_attrition %>% mutate(JobInvolvement = as.factor(
  case_when(
    JobInvolvement == 1 ~ 'Low',
    JobInvolvement == 2 ~ 'Medium',
    JobInvolvement == 3 ~ 'High',
    JobInvolvement == 4 ~ 'Very High')
)
)

employee_attrition$JobInvolvement = factor(employee_attrition$JobInvolvement, order = TRUE, 
                                                    levels = c("Low", "Medium", "High", "Very High"))

employee_attrition = employee_attrition %>% mutate(JobSatisfaction = as.factor(
  case_when(
    JobSatisfaction == 1 ~ 'Low',
    JobSatisfaction == 2 ~ 'Medium',
    JobSatisfaction == 3 ~ 'High',
    JobSatisfaction == 4 ~ 'Very High')
)
)

employee_attrition$JobSatisfaction = factor(employee_attrition$JobSatisfaction, order = TRUE, 
                                           levels = c("Low", "Medium", "High", "Very High"))

employee_attrition = employee_attrition %>% mutate(RelationshipSatisfaction = as.factor(
  case_when(
    RelationshipSatisfaction == 1 ~ 'Low',
    RelationshipSatisfaction == 2 ~ 'Medium',
    RelationshipSatisfaction == 3 ~ 'High',
    RelationshipSatisfaction == 4 ~ 'Very High')
)
)

employee_attrition$RelationshipSatisfaction = factor(employee_attrition$RelationshipSatisfaction, order = TRUE, 
                                           levels = c("Low", "Medium", "High", "Very High"))

employee_attrition = employee_attrition %>% mutate(PerformanceRating = as.factor(
  case_when(
    PerformanceRating == 1 ~ 'Low',
    PerformanceRating == 2 ~ 'Good',
    PerformanceRating == 3 ~ 'Excellent',
    PerformanceRating == 4 ~ 'Outstanding')
)
)

employee_attrition = employee_attrition %>% mutate(StockOptionLevel = as.factor(StockOptionLevel))

employee_attrition = employee_attrition %>% mutate(WorkLifeBalance = as.factor(
  case_when(
    WorkLifeBalance == 1 ~ 'Bad',
    WorkLifeBalance == 2 ~ 'Good',
    WorkLifeBalance == 3 ~ 'Better',
    WorkLifeBalance == 4 ~ 'Best')
)
)

employee_attrition$WorkLifeBalance = factor(employee_attrition$WorkLifeBalance, order = TRUE, 
                                           levels = c("Bad", "Good", "Better", "Best"))

#
# Convert character attributes to factors
#
employee_attrition = employee_attrition %>% mutate(
  BusinessTravel = as.factor(BusinessTravel), 
  Department = as.factor(Department),
  EducationField = as.factor(EducationField),
  Gender = as.factor(Gender),
  JobRole = as.factor(JobRole),
  MaritalStatus = as.factor(MaritalStatus),
  Over18 = as.factor(Over18),
  OverTime = as.factor(OverTime)
)

employee_attrition$BusinessTravel = factor(employee_attrition$BusinessTravel, order = TRUE, 
                                            levels = c("Non-Travel", "Travel_Rarely", "Travel_Frequently"))

#
#  Convert Attrition to factor
#
employee_attrition = employee_attrition %>% mutate(Attrition = as.factor(Attrition))
#
#  Convert numberic JobLevel to factor
employee_attrition = employee_attrition %>% mutate(JobLevel = as.factor(JobLevel))
#
#  Drop attributes that have the same value for all observations and the unique identifier
#
employee_attrition = employee_attrition %>% select(-Over18, -EmployeeCount, -StandardHours, -EmployeeNumber)
#
# Summary after initial data review and cleanup
#
colnames(employee_attrition %>% select_if(is.factor))
colnames(employee_attrition %>% select_if(is.numeric))
#
# univariate analysis
#
#
# Rename selected factor values to enable easier visualization
#
library(plyr)
employee_attrition$JobRole = revalue(employee_attrition$JobRole, c("Healthcare Representative" = "HealthRep",
                                                                   "Human Resources" = "HR",
                                                                   "Laboratory Technician" = "LabTech",
                                                                   "Manager" = "Mgr",
                                                                   "Manufacturing Director" = "MfgDir",
                                                                   "Research Director" = "ResearchDir", 
                                                                   "Research Scientist" = "ResearchSci", 
                                                                   "Sales Executive" = "SalesExec",
                                                                   "Sales Representative" = "SalesRep")
)

employee_attrition$EducationField = revalue(employee_attrition$EducationField, c("Human Resources" = "HR",
                                                                                 "Life Sciences" = "LifeSci",
                                                                                 "Marketing" = "Mktg",
                                                                                 "Medical" = "Med",
                                                                                 "Technical Degree" = "Tech")
)

employee_attrition$Department = revalue(employee_attrition$Department, c("Human Resources" = "HR",
                                                                         "Research & Development" = "R&D")
                                                                         
)

employee_attrition$JobRole = revalue(employee_attrition$JobRole, c("ResearchDir" = "RschDir",
                                                                         "ResearchSci" = "RschSci")
                                        
)

#
# Create bar charts of factors
#
p1 = ggplot(employee_attrition) + geom_bar(aes(x = Attrition))
p2 = ggplot(employee_attrition) + geom_bar(aes(x = Gender))
p3 = ggplot(employee_attrition) + geom_bar(aes(x = MaritalStatus))
p4 = ggplot(employee_attrition) + geom_bar(aes(x = Education))
p5 = ggplot(employee_attrition) + geom_bar(aes(x = EducationField))
p6 = ggplot(employee_attrition) + geom_bar(aes(x = PerformanceRating))
p7 = ggplot(employee_attrition) + geom_bar(aes(x = Department))
p8 = ggplot(employee_attrition) + geom_bar(aes(x = JobLevel))
p9 = ggplot(employee_attrition) + geom_bar(aes(x = StockOptionLevel))
p10 = ggplot(employee_attrition) + geom_bar(aes(x = OverTime))
p11 = ggplot(employee_attrition) + geom_bar(aes(x = JobRole))
p12 = ggplot(employee_attrition) + geom_bar(aes(x = EnvironmentSatisfaction))
p13 = ggplot(employee_attrition) + geom_bar(aes(x = JobInvolvement))
p14 = ggplot(employee_attrition) + geom_bar(aes(x = JobSatisfaction))
p15 = ggplot(employee_attrition) + geom_bar(aes(x = RelationshipSatisfaction))
p16 = ggplot(employee_attrition) + geom_bar(aes(x = WorkLifeBalance))
p17 = ggplot(employee_attrition) + geom_bar(aes(x = BusinessTravel))

grid.arrange(p2, p3, p4, p5, nrow=2, top = "Demographic and Education Factors")
grid.arrange(p6, p7, p8, p9, p10, p11, nrow=2, top = "Organizational Factors")
grid.arrange(p12, p13, p14, p15, p16, p17, nrow=2, top = "Personal Factors")
#
#  Univariate Analysis of Numeric Attributes
#
#  Demographic Attributes
#
p1 = ggplot(employee_attrition) + geom_histogram(aes(x = Age))
p2 = ggplot(employee_attrition) + geom_histogram(aes(x = DistanceFromHome))
p3 = ggplot(employee_attrition) + geom_histogram(aes(x = NumCompaniesWorked))
grid.arrange(p1, p2, p3, nrow=1, top = "Demographic Measures")
#
# Compensation
#
summarize_numeric(employee_attrition %>% select(HourlyRate, DailyRate, MonthlyRate, MonthlyIncome, PercentSalaryHike))
p_comp1 = ggplot(employee_attrition) + geom_histogram(aes(x = DailyRate))
p_comp2 = ggplot(employee_attrition) + geom_histogram(aes(x = HourlyRate))
p_comp3 = ggplot(employee_attrition) + geom_histogram(aes(x = MonthlyRate))
p_comp4 = ggplot(employee_attrition) + geom_histogram(aes(x = MonthlyIncome))
p_comp5 = ggplot(employee_attrition) + geom_histogram(aes(x = PercentSalaryHike))
grid.arrange(p_comp1, p_comp2, p_comp3, p_comp4, nrow = 1, top = "Compensation Measures")
#
# Organizational
#
summarize_numeric(employee_attrition %>% select(TrainingTimesLastYear, PercentSalaryHike))
p1 = ggplot(employee_attrition) + geom_histogram(aes(x = TrainingTimesLastYear))
p2 = ggplot(employee_attrition) + geom_histogram(aes(x = PercentSalaryHike))
grid.arrange(p1, p2, nrow = 1, top = "Organizational Measures")
#
# Tenure
#
summarize_numeric(select(employee_attrition, YearsAtCompany, YearsInCurrentRole, YearsSinceLastPromotion, YearsWithCurrManager, TotalWorkingYears))
p_tenure1 = ggplot(employee_attrition) + geom_histogram(aes(x = YearsAtCompany))
p_tenure2 = ggplot(employee_attrition) + geom_histogram(aes(x = YearsInCurrentRole))
p_tenure3 = ggplot(employee_attrition) + geom_histogram(aes(x = YearsSinceLastPromotion))
p_tenure4 = ggplot(employee_attrition) + geom_histogram(aes(x = YearsWithCurrManager))
p_tenure5 = ggplot(employee_attrition) + geom_histogram(aes(x = TotalWorkingYears))
grid.arrange(p_tenure1, p_tenure2, p_tenure3, p_tenure4, p_tenure5, nrow = 2, top = "Tenure Measures")
#
# Summarize
#
colnames(employee_attrition %>% select_if(is.factor))
colnames(employee_attrition %>% select_if(is.numeric))

g1 = ggplot(attrition_income_data) + geom_histogram(aes(x=MonthlyRate_Hourly_Ratio))
g2 = ggplot(attrition_income_data) + geom_histogram(aes(x=MonthlyIncome_MonthlyRate_ratio))
g3 = ggplot(attrition_income_data) + geom_histogram(aes(x=MonthlyRate_Hourly_Ratio))
grid.arrange(g1, g2, g3, nrow = 1)

g1 = ggplot(employee_attrition) + geom_boxplot(aes(x=Attrition, y=MonthlyIncome))
g2 = ggplot(employee_attrition) + geom_boxplot(aes(x=Attrition, y=MonthlyRate))
g3 = ggplot(employee_attrition) + geom_boxplot(aes(x=Attrition, y=HourlyRate))
grid.arrange(g1, g2, g3, nrow=1)

g1 = ggplot(employee_attrition) + geom_boxplot(aes(x=Education, y=MonthlyIncome))
g2 = ggplot(employee_attrition) + geom_boxplot(aes(x=Education, y=MonthlyRate))
g3 = ggplot(employee_attrition) + geom_boxplot(aes(x=Education, y=HourlyRate))

g4 = ggplot(employee_attrition) + geom_boxplot(aes(x=Department, y=MonthlyIncome))
g5 = ggplot(employee_attrition) + geom_boxplot(aes(x=Department, y=MonthlyRate))
g6 = ggplot(employee_attrition) + geom_boxplot(aes(x=Department, y=HourlyRate))

g7 = ggplot(employee_attrition) + geom_boxplot(aes(x=JobRole, y=MonthlyIncome))
g8 = ggplot(employee_attrition) + geom_boxplot(aes(x=JobRole, y=MonthlyRate))
g9 = ggplot(employee_attrition) + geom_boxplot(aes(x=JobRole, y=HourlyRate))

g10 = ggplot(employee_attrition) + geom_boxplot(aes(x=JobLevel, y=MonthlyIncome))
g11 = ggplot(employee_attrition) + geom_boxplot(aes(x=JobLevel, y=MonthlyRate))
g12 = ggplot(employee_attrition) + geom_boxplot(aes(x=JobLevel, y=HourlyRate))

grid.arrange(g1, g4, g7, g10, nrow=4)
grid.arrange(g2, g5, g8, g11, nrow=4)
grid.arrange(g3, g6, g9, g12, nrow=4)

# grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, nrow=4)
#
#  Remove apparently meaningless rate attributes
#
employee_attrition = employee_attrition %>% select(-MonthlyRate, -HourlyRate, -DailyRate)
#
#  Understanding JobLevel, OverTime, and StockOptionLevel
#
employee_attrition %>% crosstab(JobLevel, JobRole)
employee_attrition %>% crosstab(JobLevel, OverTime)
employee_attrition %>% crosstab(JobLevel, StockOptionLevel)
employee_attrition %>% crosstab(JobLevel, Department)
employee_attrition %>% crosstab(JobLevel, EducationField)
employee_attrition %>% crosstab(Department, EducationField)

g1 = ggplot(employee_attrition) + geom_boxplot(aes(x= OverTime, y=Age)) 
g2 = ggplot(employee_attrition) + geom_boxplot(aes(x= OverTime, y=DistanceFromHome)) 
g3 = ggplot(employee_attrition) + geom_boxplot(aes(x= OverTime, y=MonthlyIncome)) 
g4 = ggplot(employee_attrition) + geom_boxplot(aes(x= OverTime, y=NumCompaniesWorked)) 
g5 = ggplot(employee_attrition) + geom_boxplot(aes(x= OverTime, y=PercentSalaryHike)) 
g6 = ggplot(employee_attrition) + geom_boxplot(aes(x= OverTime, y=TotalWorkingYears)) 
g7 = ggplot(employee_attrition) + geom_boxplot(aes(x= OverTime, y=TrainingTimesLastYear)) 
g8 = ggplot(employee_attrition) + geom_boxplot(aes(x= OverTime, y=YearsAtCompany)) 
g9 = ggplot(employee_attrition) + geom_boxplot(aes(x= OverTime, y=YearsInCurrentRole)) 
g10 = ggplot(employee_attrition) + geom_boxplot(aes(x= OverTime, y=YearsSinceLastPromotion)) 
g11 = ggplot(employee_attrition) + geom_boxplot(aes(x= OverTime, y=YearsWithCurrManager)) 
grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, nrow=4)

g1 = ggplot(employee_attrition) + geom_bar(aes(x=Gender, fill = OverTime), position = "fill") + labs(y = "Percent")
g2 = ggplot(employee_attrition) + geom_bar(aes(x=MaritalStatus, fill = OverTime), position = "fill") + labs(y = "Percent")
g3 = ggplot(employee_attrition) + geom_bar(aes(x=Education, fill = OverTime), position = "fill") + labs(y = "Percent")
g4 = ggplot(employee_attrition) + geom_bar(aes(x=EducationField, fill = OverTime), position = "fill") + labs(y = "Percent")
g5 = ggplot(employee_attrition) + geom_bar(aes(x=PerformanceRating, fill = OverTime), position = "fill") + labs(y = "Percent")
g6 = ggplot(employee_attrition) + geom_bar(aes(x=Department, fill = OverTime), position = "fill") + labs(y = "Percent")
g7 = ggplot(employee_attrition) + geom_bar(aes(x=StockOptionLevel, fill = OverTime), position = "fill") + labs(y = "Percent")
g8 = ggplot(employee_attrition) + geom_bar(aes(x=JobLevel, fill = OverTime), position = "fill") + labs(y = "Percent")
g9 = ggplot(employee_attrition) + geom_bar(aes(x=JobRole, fill = OverTime), position = "fill") + labs(y = "Percent")
g10 = ggplot(employee_attrition) + geom_bar(aes(x=EnvironmentSatisfaction, fill = OverTime), position = "fill") + labs(y = "Percent")
g11 = ggplot(employee_attrition) + geom_bar(aes(x=JobInvolvement, fill = OverTime), position = "fill") + labs(y = "Percent")
g12 = ggplot(employee_attrition) + geom_bar(aes(x=JobSatisfaction, fill = OverTime), position = "fill") + labs(y = "Percent")
g13 = ggplot(employee_attrition) + geom_bar(aes(x=RelationshipSatisfaction, fill = OverTime), position = "fill") + labs(y = "Percent")
g14 = ggplot(employee_attrition) + geom_bar(aes(x=WorkLifeBalance, fill = OverTime), position = "fill") + labs(y = "Percent")
g15 = ggplot(employee_attrition) + geom_bar(aes(x=BusinessTravel, fill = OverTime), position = "fill") + labs(y = "Percent")

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, g14, g15, nrow=5)

g1 = ggplot(employee_attrition) + geom_boxplot(aes(x= StockOptionLevel, y=Age)) 
g2 = ggplot(employee_attrition) + geom_boxplot(aes(x= StockOptionLevel, y=DistanceFromHome)) 
g3 = ggplot(employee_attrition) + geom_boxplot(aes(x= StockOptionLevel, y=MonthlyIncome)) 
g4 = ggplot(employee_attrition) + geom_boxplot(aes(x= StockOptionLevel, y=NumCompaniesWorked)) 
g5 = ggplot(employee_attrition) + geom_boxplot(aes(x= StockOptionLevel, y=PercentSalaryHike)) 
g6 = ggplot(employee_attrition) + geom_boxplot(aes(x= StockOptionLevel, y=TotalWorkingYears)) 
g7 = ggplot(employee_attrition) + geom_boxplot(aes(x= StockOptionLevel, y=TrainingTimesLastYear)) 
g8 = ggplot(employee_attrition) + geom_boxplot(aes(x= StockOptionLevel, y=YearsAtCompany)) 
g9 = ggplot(employee_attrition) + geom_boxplot(aes(x= StockOptionLevel, y=YearsInCurrentRole)) 
g10 = ggplot(employee_attrition) + geom_boxplot(aes(x= StockOptionLevel, y=YearsSinceLastPromotion)) 
g11 = ggplot(employee_attrition) + geom_boxplot(aes(x= StockOptionLevel, y=YearsWithCurrManager)) 

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, nrow=4)

g1 = ggplot(employee_attrition) + geom_bar(aes(x=Gender, fill = StockOptionLevel), position = "fill") + labs(y = "Percent")
g2 = ggplot(employee_attrition) + geom_bar(aes(x=MaritalStatus, fill = StockOptionLevel), position = "fill") + labs(y = "Percent")
g3 = ggplot(employee_attrition) + geom_bar(aes(x=Education, fill = StockOptionLevel), position = "fill") + labs(y = "Percent")
g4 = ggplot(employee_attrition) + geom_bar(aes(x=EducationField, fill = StockOptionLevel), position = "fill") + labs(y = "Percent")
g5 = ggplot(employee_attrition) + geom_bar(aes(x=PerformanceRating, fill = StockOptionLevel), position = "fill") + labs(y = "Percent")
g6 = ggplot(employee_attrition) + geom_bar(aes(x=Department, fill = StockOptionLevel), position = "fill") + labs(y = "Percent")
g7 = ggplot(employee_attrition) + geom_bar(aes(x=OverTime, fill = StockOptionLevel), position = "fill") + labs(y = "Percent")
g8 = ggplot(employee_attrition) + geom_bar(aes(x=JobLevel, fill = StockOptionLevel), position = "fill") + labs(y = "Percent")
g9 = ggplot(employee_attrition) + geom_bar(aes(x=JobRole, fill = StockOptionLevel), position = "fill") + labs(y = "Percent")
g10 = ggplot(employee_attrition) + geom_bar(aes(x=EnvironmentSatisfaction, fill = StockOptionLevel), position = "fill") + labs(y = "Percent")
g11 = ggplot(employee_attrition) + geom_bar(aes(x=JobInvolvement, fill = StockOptionLevel), position = "fill") + labs(y = "Percent")
g12 = ggplot(employee_attrition) + geom_bar(aes(x=JobSatisfaction, fill = StockOptionLevel), position = "fill") + labs(y = "Percent")
g13 = ggplot(employee_attrition) + geom_bar(aes(x=RelationshipSatisfaction, fill = StockOptionLevel), position = "fill") + labs(y = "Percent")
g14 = ggplot(employee_attrition) + geom_bar(aes(x=WorkLifeBalance, fill = StockOptionLevel), position = "fill") + labs(y = "Percent")
g15 = ggplot(employee_attrition) + geom_bar(aes(x=BusinessTravel, fill = StockOptionLevel), position = "fill") + labs(y = "Percent")

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, g14, g15, nrow=5)
#
#  Remove OverTime and StockOptionLevel attributes
#
employee_attrition = employee_attrition %>% select(-OverTime, -StockOptionLevel)

#########################################################################################################################
#                                                                                                                       #
#  Bivariate Analysis                                                                                                   #
#                                                                                                                       #
#########################################################################################################################
#
#  Measures vs Measures
#
fullCorrMatrix = round(cor(employee_attrition %>% select_if(is.numeric)), 2)
ggcorrplot(fullCorrMatrix)

g1 = ggplot(employee_attrition) + geom_point(aes(x=Age, y = MonthlyIncome))
g2 = ggplot(employee_attrition) + geom_point(aes(x=Age, y = NumCompaniesWorked))
g3 = ggplot(employee_attrition) + geom_point(aes(x=Age, y = DistanceFromHome))
g4 = ggplot(employee_attrition) + geom_point(aes(x=Age, y = YearsAtCompany))
grid.arrange(g1, g2, g3, g4, nrow=2)
#
#  Categories vs Measures
#
g1 = ggplot(employee_attrition) + geom_boxplot(aes(x= Gender, y=Age)) + theme(axis.title.y = element_blank())
g2 = ggplot(employee_attrition) + geom_boxplot(aes(x= MaritalStatus, y=Age)) + theme(axis.title.y = element_blank())
g3 = ggplot(employee_attrition) + geom_boxplot(aes(x= Education, y=Age)) + theme(axis.title.y = element_blank())
g4 = ggplot(employee_attrition) + geom_boxplot(aes(x= EducationField, y=Age)) + theme(axis.title.y = element_blank())
g5 = ggplot(employee_attrition) + geom_boxplot(aes(x= PerformanceRating, y=Age)) + theme(axis.title.y = element_blank())
g6 = ggplot(employee_attrition) + geom_boxplot(aes(x= Department, y=Age)) + theme(axis.title.y = element_blank())
g7 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobLevel, y=Age)) + theme(axis.title.y = element_blank())
g8 = ggplot(employee_attrition) + geom_boxplot(aes(x= BusinessTravel, y=Age)) + theme(axis.title.y = element_blank())
g9 = ggplot(employee_attrition) + geom_boxplot(aes(x= EnvironmentSatisfaction, y=Age)) + theme(axis.title.y = element_blank())
g10 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobInvolvement, y=Age)) + theme(axis.title.y = element_blank())
g11 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobSatisfaction, y=Age)) + theme(axis.title.y = element_blank())
g12 = ggplot(employee_attrition) + geom_boxplot(aes(x= RelationshipSatisfaction, y=Age)) + theme(axis.title.y = element_blank())
g13 = ggplot(employee_attrition) + geom_boxplot(aes(x= WorkLifeBalance, y=Age)) + theme(axis.title.y = element_blank())

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, nrow=3)

g1 = ggplot(employee_attrition) + geom_boxplot(aes(x= Gender, y=DistanceFromHome)) + theme(axis.title.y = element_blank())
g2 = ggplot(employee_attrition) + geom_boxplot(aes(x= MaritalStatus, y=DistanceFromHome)) + theme(axis.title.y = element_blank())
g3 = ggplot(employee_attrition) + geom_boxplot(aes(x= Education, y=DistanceFromHome)) + theme(axis.title.y = element_blank())
g4 = ggplot(employee_attrition) + geom_boxplot(aes(x= EducationField, y=DistanceFromHome)) + theme(axis.title.y = element_blank())
g5 = ggplot(employee_attrition) + geom_boxplot(aes(x= PerformanceRating, y=DistanceFromHome)) + theme(axis.title.y = element_blank())
g6 = ggplot(employee_attrition) + geom_boxplot(aes(x= Department, y=DistanceFromHome)) + theme(axis.title.y = element_blank())
g7 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobLevel, y=DistanceFromHome)) + theme(axis.title.y = element_blank())
g8 = ggplot(employee_attrition) + geom_boxplot(aes(x= BusinessTravel, y=DistanceFromHome)) + theme(axis.title.y = element_blank())
g9 = ggplot(employee_attrition) + geom_boxplot(aes(x= EnvironmentSatisfaction, y=DistanceFromHome)) + theme(axis.title.y = element_blank())
g10 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobInvolvement, y=DistanceFromHome)) + theme(axis.title.y = element_blank())
g11 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobSatisfaction, y=DistanceFromHome)) + theme(axis.title.y = element_blank())
g12 = ggplot(employee_attrition) + geom_boxplot(aes(x= RelationshipSatisfaction, y=DistanceFromHome)) + theme(axis.title.y = element_blank())
g13 = ggplot(employee_attrition) + geom_boxplot(aes(x= WorkLifeBalance, y=DistanceFromHome)) + theme(axis.title.y = element_blank())

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13,nrow=3)

g1 = ggplot(employee_attrition) + geom_boxplot(aes(x= Gender, y=NumCompaniesWorked)) + theme(axis.title.y = element_blank())
g2 = ggplot(employee_attrition) + geom_boxplot(aes(x= MaritalStatus, y=NumCompaniesWorked)) + theme(axis.title.y = element_blank())
g3 = ggplot(employee_attrition) + geom_boxplot(aes(x= Education, y=NumCompaniesWorked)) + theme(axis.title.y = element_blank())
g4 = ggplot(employee_attrition) + geom_boxplot(aes(x= EducationField, y=NumCompaniesWorked)) + theme(axis.title.y = element_blank())
g5 = ggplot(employee_attrition) + geom_boxplot(aes(x= PerformanceRating, y=NumCompaniesWorked)) + theme(axis.title.y = element_blank())
g6 = ggplot(employee_attrition) + geom_boxplot(aes(x= Department, y=NumCompaniesWorked)) + theme(axis.title.y = element_blank())
g7 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobLevel, y=NumCompaniesWorked)) + theme(axis.title.y = element_blank())
g8 = ggplot(employee_attrition) + geom_boxplot(aes(x= BusinessTravel, y=NumCompaniesWorked)) + theme(axis.title.y = element_blank())
g9 = ggplot(employee_attrition) + geom_boxplot(aes(x= EnvironmentSatisfaction, y=NumCompaniesWorked)) + theme(axis.title.y = element_blank())
g10 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobInvolvement, y=NumCompaniesWorked)) + theme(axis.title.y = element_blank())
g11 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobSatisfaction, y=NumCompaniesWorked)) + theme(axis.title.y = element_blank())
g12 = ggplot(employee_attrition) + geom_boxplot(aes(x= RelationshipSatisfaction, y=NumCompaniesWorked)) + theme(axis.title.y = element_blank())
g13 = ggplot(employee_attrition) + geom_boxplot(aes(x= WorkLifeBalance, y=NumCompaniesWorked)) + theme(axis.title.y = element_blank())

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13,nrow=3)

g1 = ggplot(employee_attrition) + geom_boxplot(aes(x= Gender, y=MonthlyIncome)) + theme(axis.title.y = element_blank())
g2 = ggplot(employee_attrition) + geom_boxplot(aes(x= MaritalStatus, y=MonthlyIncome)) + theme(axis.title.y = element_blank())
g3 = ggplot(employee_attrition) + geom_boxplot(aes(x= Education, y=MonthlyIncome)) + theme(axis.title.y = element_blank())
g4 = ggplot(employee_attrition) + geom_boxplot(aes(x= EducationField, y=MonthlyIncome)) + theme(axis.title.y = element_blank())
g5 = ggplot(employee_attrition) + geom_boxplot(aes(x= PerformanceRating, y=MonthlyIncome)) + theme(axis.title.y = element_blank())
g6 = ggplot(employee_attrition) + geom_boxplot(aes(x= Department, y=MonthlyIncome)) + theme(axis.title.y = element_blank())
g7 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobLevel, y=MonthlyIncome)) + theme(axis.title.y = element_blank())
g8 = ggplot(employee_attrition) + geom_boxplot(aes(x= BusinessTravel, y=MonthlyIncome)) + theme(axis.title.y = element_blank())
g9 = ggplot(employee_attrition) + geom_boxplot(aes(x= EnvironmentSatisfaction, y=MonthlyIncome)) + theme(axis.title.y = element_blank())
g10 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobInvolvement, y=MonthlyIncome)) + theme(axis.title.y = element_blank())
g11 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobSatisfaction, y=MonthlyIncome)) + theme(axis.title.y = element_blank())
g12 = ggplot(employee_attrition) + geom_boxplot(aes(x= RelationshipSatisfaction, y=MonthlyIncome)) + theme(axis.title.y = element_blank())
g13 = ggplot(employee_attrition) + geom_boxplot(aes(x= WorkLifeBalance, y=MonthlyIncome)) + theme(axis.title.y = element_blank())

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, nrow=3)

g1 = ggplot(employee_attrition) + geom_boxplot(aes(x= Gender, y=TrainingTimesLastYear)) + theme(axis.title.y = element_blank())
g2 = ggplot(employee_attrition) + geom_boxplot(aes(x= MaritalStatus, y=TrainingTimesLastYear)) + theme(axis.title.y = element_blank())
g3 = ggplot(employee_attrition) + geom_boxplot(aes(x= Education, y=TrainingTimesLastYear)) + theme(axis.title.y = element_blank())
g4 = ggplot(employee_attrition) + geom_boxplot(aes(x= EducationField, y=TrainingTimesLastYear)) + theme(axis.title.y = element_blank())
g5 = ggplot(employee_attrition) + geom_boxplot(aes(x= PerformanceRating, y=TrainingTimesLastYear)) + theme(axis.title.y = element_blank())
g6 = ggplot(employee_attrition) + geom_boxplot(aes(x= Department, y=TrainingTimesLastYear)) + theme(axis.title.y = element_blank())
g7 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobLevel, y=TrainingTimesLastYear)) + theme(axis.title.y = element_blank())
g8 = ggplot(employee_attrition) + geom_boxplot(aes(x= BusinessTravel, y=TrainingTimesLastYear)) + theme(axis.title.y = element_blank())
g9 = ggplot(employee_attrition) + geom_boxplot(aes(x= EnvironmentSatisfaction, y=TrainingTimesLastYear)) + theme(axis.title.y = element_blank())
g10 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobInvolvement, y=TrainingTimesLastYear)) + theme(axis.title.y = element_blank())
g11 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobSatisfaction, y=TrainingTimesLastYear)) + theme(axis.title.y = element_blank())
g12 = ggplot(employee_attrition) + geom_boxplot(aes(x= RelationshipSatisfaction, y=TrainingTimesLastYear)) + theme(axis.title.y = element_blank())
g13 = ggplot(employee_attrition) + geom_boxplot(aes(x= WorkLifeBalance, y=TrainingTimesLastYear)) + theme(axis.title.y = element_blank())

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, nrow=3)

g1 = ggplot(employee_attrition) + geom_boxplot(aes(x= Gender, y=PercentSalaryHike)) + theme(axis.title.y = element_blank())
g2 = ggplot(employee_attrition) + geom_boxplot(aes(x= MaritalStatus, y=PercentSalaryHike)) + theme(axis.title.y = element_blank())
g3 = ggplot(employee_attrition) + geom_boxplot(aes(x= Education, y=PercentSalaryHike)) + theme(axis.title.y = element_blank())
g4 = ggplot(employee_attrition) + geom_boxplot(aes(x= EducationField, y=PercentSalaryHike)) + theme(axis.title.y = element_blank())
g5 = ggplot(employee_attrition) + geom_boxplot(aes(x= PerformanceRating, y=PercentSalaryHike)) + theme(axis.title.y = element_blank())
g6 = ggplot(employee_attrition) + geom_boxplot(aes(x= Department, y=PercentSalaryHike)) + theme(axis.title.y = element_blank())
g7 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobLevel, y=PercentSalaryHike)) + theme(axis.title.y = element_blank())
g8 = ggplot(employee_attrition) + geom_boxplot(aes(x= BusinessTravel, y=PercentSalaryHike)) + theme(axis.title.y = element_blank())
g9 = ggplot(employee_attrition) + geom_boxplot(aes(x= EnvironmentSatisfaction, y=PercentSalaryHike)) + theme(axis.title.y = element_blank())
g10 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobInvolvement, y=PercentSalaryHike)) + theme(axis.title.y = element_blank())
g11 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobSatisfaction, y=PercentSalaryHike)) + theme(axis.title.y = element_blank())
g12 = ggplot(employee_attrition) + geom_boxplot(aes(x= RelationshipSatisfaction, y=PercentSalaryHike)) + theme(axis.title.y = element_blank())
g13 = ggplot(employee_attrition) + geom_boxplot(aes(x= WorkLifeBalance, y=PercentSalaryHike)) + theme(axis.title.y = element_blank())

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, nrow=3)

g1 = ggplot(employee_attrition) + geom_boxplot(aes(x= Gender, y=YearsAtCompany)) + theme(axis.title.y = element_blank())
g2 = ggplot(employee_attrition) + geom_boxplot(aes(x= MaritalStatus, y=YearsAtCompany)) + theme(axis.title.y = element_blank())
g3 = ggplot(employee_attrition) + geom_boxplot(aes(x= Education, y=YearsAtCompany)) + theme(axis.title.y = element_blank())
g4 = ggplot(employee_attrition) + geom_boxplot(aes(x= EducationField, y=YearsAtCompany)) + theme(axis.title.y = element_blank())
g5 = ggplot(employee_attrition) + geom_boxplot(aes(x= PerformanceRating, y=YearsAtCompany)) + theme(axis.title.y = element_blank())
g6 = ggplot(employee_attrition) + geom_boxplot(aes(x= Department, y=YearsAtCompany)) + theme(axis.title.y = element_blank())
g7 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobLevel, y=YearsAtCompany)) + theme(axis.title.y = element_blank())
g8 = ggplot(employee_attrition) + geom_boxplot(aes(x= BusinessTravel, y=YearsAtCompany)) + theme(axis.title.y = element_blank())
g9 = ggplot(employee_attrition) + geom_boxplot(aes(x= EnvironmentSatisfaction, y=YearsAtCompany)) + theme(axis.title.y = element_blank())
g10 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobInvolvement, y=YearsAtCompany)) + theme(axis.title.y = element_blank())
g11 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobSatisfaction, y=YearsAtCompany)) + theme(axis.title.y = element_blank())
g12 = ggplot(employee_attrition) + geom_boxplot(aes(x= RelationshipSatisfaction, y=YearsAtCompany)) + theme(axis.title.y = element_blank())
g13 = ggplot(employee_attrition) + geom_boxplot(aes(x= WorkLifeBalance, y=YearsAtCompany)) + theme(axis.title.y = element_blank())

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, nrow=3)

g1 = ggplot(employee_attrition) + geom_boxplot(aes(x= Gender, y=YearsInCurrentRole)) + theme(axis.title.y = element_blank())
g2 = ggplot(employee_attrition) + geom_boxplot(aes(x= MaritalStatus, y=YearsInCurrentRole)) + theme(axis.title.y = element_blank())
g3 = ggplot(employee_attrition) + geom_boxplot(aes(x= Education, y=YearsInCurrentRole)) + theme(axis.title.y = element_blank())
g4 = ggplot(employee_attrition) + geom_boxplot(aes(x= EducationField, y=YearsInCurrentRole)) + theme(axis.title.y = element_blank())
g5 = ggplot(employee_attrition) + geom_boxplot(aes(x= PerformanceRating, y=YearsInCurrentRole)) + theme(axis.title.y = element_blank())
g6 = ggplot(employee_attrition) + geom_boxplot(aes(x= Department, y=YearsInCurrentRole)) + theme(axis.title.y = element_blank())
g7 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobLevel, y=YearsInCurrentRole)) + theme(axis.title.y = element_blank())
g8 = ggplot(employee_attrition) + geom_boxplot(aes(x= BusinessTravel, y=YearsInCurrentRole)) + theme(axis.title.y = element_blank())
g9 = ggplot(employee_attrition) + geom_boxplot(aes(x= EnvironmentSatisfaction, y=YearsInCurrentRole)) + theme(axis.title.y = element_blank())
g10 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobInvolvement, y=YearsInCurrentRole)) + theme(axis.title.y = element_blank())
g11 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobSatisfaction, y=YearsInCurrentRole)) + theme(axis.title.y = element_blank())
g12 = ggplot(employee_attrition) + geom_boxplot(aes(x= RelationshipSatisfaction, y=YearsInCurrentRole)) + theme(axis.title.y = element_blank())
g13 = ggplot(employee_attrition) + geom_boxplot(aes(x= WorkLifeBalance, y=YearsInCurrentRole)) + theme(axis.title.y = element_blank())

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, nrow=3)

g1 = ggplot(employee_attrition) + geom_boxplot(aes(x= Gender, y=YearsSinceLastPromotion)) + theme(axis.title.y = element_blank())
g2 = ggplot(employee_attrition) + geom_boxplot(aes(x= MaritalStatus, y=YearsSinceLastPromotion)) + theme(axis.title.y = element_blank())
g3 = ggplot(employee_attrition) + geom_boxplot(aes(x= Education, y=YearsSinceLastPromotion)) + theme(axis.title.y = element_blank())
g4 = ggplot(employee_attrition) + geom_boxplot(aes(x= EducationField, y=YearsSinceLastPromotion)) + theme(axis.title.y = element_blank())
g5 = ggplot(employee_attrition) + geom_boxplot(aes(x= PerformanceRating, y=YearsSinceLastPromotion)) + theme(axis.title.y = element_blank())
g6 = ggplot(employee_attrition) + geom_boxplot(aes(x= Department, y=YearsSinceLastPromotion)) + theme(axis.title.y = element_blank())
g7 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobLevel, y=YearsSinceLastPromotion)) + theme(axis.title.y = element_blank())
g8 = ggplot(employee_attrition) + geom_boxplot(aes(x= BusinessTravel, y=YearsSinceLastPromotion)) + theme(axis.title.y = element_blank())
g9 = ggplot(employee_attrition) + geom_boxplot(aes(x= EnvironmentSatisfaction, y=YearsSinceLastPromotion)) + theme(axis.title.y = element_blank())
g10 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobInvolvement, y=YearsSinceLastPromotion)) + theme(axis.title.y = element_blank())
g11 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobSatisfaction, y=YearsSinceLastPromotion)) + theme(axis.title.y = element_blank())
g12 = ggplot(employee_attrition) + geom_boxplot(aes(x= RelationshipSatisfaction, y=YearsSinceLastPromotion)) + theme(axis.title.y = element_blank())
g13 = ggplot(employee_attrition) + geom_boxplot(aes(x= WorkLifeBalance, y=YearsSinceLastPromotion)) + theme(axis.title.y = element_blank())

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, nrow=3)

g1 = ggplot(employee_attrition) + geom_boxplot(aes(x= Gender, y=YearsWithCurrManager)) + theme(axis.title.y = element_blank())
g2 = ggplot(employee_attrition) + geom_boxplot(aes(x= MaritalStatus, y=YearsWithCurrManager)) + theme(axis.title.y = element_blank())
g3 = ggplot(employee_attrition) + geom_boxplot(aes(x= Education, y=YearsWithCurrManager)) + theme(axis.title.y = element_blank())
g4 = ggplot(employee_attrition) + geom_boxplot(aes(x= EducationField, y=YearsWithCurrManager)) + theme(axis.title.y = element_blank())
g5 = ggplot(employee_attrition) + geom_boxplot(aes(x= PerformanceRating, y=YearsWithCurrManager)) + theme(axis.title.y = element_blank())
g6 = ggplot(employee_attrition) + geom_boxplot(aes(x= Department, y=YearsWithCurrManager)) + theme(axis.title.y = element_blank())
g7 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobLevel, y=YearsWithCurrManager)) + theme(axis.title.y = element_blank())
g8 = ggplot(employee_attrition) + geom_boxplot(aes(x= BusinessTravel, y=YearsWithCurrManager)) + theme(axis.title.y = element_blank())
g9 = ggplot(employee_attrition) + geom_boxplot(aes(x= EnvironmentSatisfaction, y=YearsWithCurrManager)) + theme(axis.title.y = element_blank())
g10 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobInvolvement, y=YearsWithCurrManager)) + theme(axis.title.y = element_blank())
g11 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobSatisfaction, y=YearsWithCurrManager)) + theme(axis.title.y = element_blank())
g12 = ggplot(employee_attrition) + geom_boxplot(aes(x= RelationshipSatisfaction, y=YearsWithCurrManager)) + theme(axis.title.y = element_blank())
g13 = ggplot(employee_attrition) + geom_boxplot(aes(x= WorkLifeBalance, y=YearsWithCurrManager)) + theme(axis.title.y = element_blank())

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, nrow=3)

g1 = ggplot(employee_attrition) + geom_boxplot(aes(x= Gender, y=TotalWorkingYears)) + theme(axis.title.y = element_blank())
g2 = ggplot(employee_attrition) + geom_boxplot(aes(x= MaritalStatus, y=TotalWorkingYears)) + theme(axis.title.y = element_blank())
g3 = ggplot(employee_attrition) + geom_boxplot(aes(x= Education, y=TotalWorkingYears)) + theme(axis.title.y = element_blank())
g4 = ggplot(employee_attrition) + geom_boxplot(aes(x= EducationField, y=TotalWorkingYears)) + theme(axis.title.y = element_blank())
g5 = ggplot(employee_attrition) + geom_boxplot(aes(x= PerformanceRating, y=TotalWorkingYears)) + theme(axis.title.y = element_blank())
g6 = ggplot(employee_attrition) + geom_boxplot(aes(x= Department, y=TotalWorkingYears)) + theme(axis.title.y = element_blank())
g7 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobLevel, y=TotalWorkingYears)) + theme(axis.title.y = element_blank())
g8 = ggplot(employee_attrition) + geom_boxplot(aes(x= BusinessTravel, y=TotalWorkingYears)) + theme(axis.title.y = element_blank())
g9 = ggplot(employee_attrition) + geom_boxplot(aes(x= EnvironmentSatisfaction, y=TotalWorkingYears)) + theme(axis.title.y = element_blank())
g10 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobInvolvement, y=TotalWorkingYears)) + theme(axis.title.y = element_blank())
g11 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobSatisfaction, y=TotalWorkingYears)) + theme(axis.title.y = element_blank())
g12 = ggplot(employee_attrition) + geom_boxplot(aes(x= RelationshipSatisfaction, y=TotalWorkingYears)) + theme(axis.title.y = element_blank())
g13 = ggplot(employee_attrition) + geom_boxplot(aes(x= WorkLifeBalance, y=TotalWorkingYears)) + theme(axis.title.y = element_blank())

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, nrow=3)

g1 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobRole, y=Age)) 
g2 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobRole, y=DistanceFromHome)) 
g3 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobRole, y=NumCompaniesWorked)) 
g4 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobRole, y=MonthlyIncome)) 
g5 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobRole, y=TrainingTimesLastYear))
g6 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobRole, y=PercentSalaryHike)) 
g7 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobRole, y=YearsAtCompany)) 
g8 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobRole, y=YearsInCurrentRole)) 
g9 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobRole, y=YearsSinceLastPromotion)) 
g10 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobRole, y=YearsWithCurrManager)) 
g11 = ggplot(employee_attrition) + geom_boxplot(aes(x= JobRole, y=TotalWorkingYears)) 

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, nrow=4)

#
# Categories vs Categories
#
g1 = ggplot(employee_attrition) + geom_bar(aes(x=Gender, fill = Gender), position = "fill") + labs(y = "Percent")
g2 = ggplot(employee_attrition) + geom_bar(aes(x=MaritalStatus, fill = Gender), position = "fill") + labs(y = "Percent")
g3 = ggplot(employee_attrition) + geom_bar(aes(x=Education, fill = Gender), position = "fill") + labs(y = "Percent")
g4 = ggplot(employee_attrition) + geom_bar(aes(x=EducationField, fill = Gender), position = "fill") + labs(y = "Percent")
g5 = ggplot(employee_attrition) + geom_bar(aes(x=PerformanceRating, fill = Gender), position = "fill") + labs(y = "Percent")
g6 = ggplot(employee_attrition) + geom_bar(aes(x=Department, fill = Gender), position = "fill") + labs(y = "Percent")
g7 = ggplot(employee_attrition) + geom_bar(aes(x=JobLevel, fill = Gender), position = "fill") + labs(y = "Percent")
g8 = ggplot(employee_attrition) + geom_bar(aes(x=JobRole, fill = Gender), position = "fill") + labs(y = "Percent")
g9 = ggplot(employee_attrition) + geom_bar(aes(x=EnvironmentSatisfaction, fill = Gender), position = "fill") + labs(y = "Percent")
g10 = ggplot(employee_attrition) + geom_bar(aes(x=JobInvolvement, fill = Gender), position = "fill") + labs(y = "Percent")
g11 = ggplot(employee_attrition) + geom_bar(aes(x=JobSatisfaction, fill = Gender), position = "fill") + labs(y = "Percent")
g12 = ggplot(employee_attrition) + geom_bar(aes(x=RelationshipSatisfaction, fill = Gender), position = "fill") + labs(y = "Percent")
g13 = ggplot(employee_attrition) + geom_bar(aes(x=WorkLifeBalance, fill = Gender), position = "fill") + labs(y = "Percent")
g14 = ggplot(employee_attrition) + geom_bar(aes(x=BusinessTravel, fill = Gender), position = "fill") + labs(y = "Percent")

grid.arrange(g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, g14,  nrow=5, top = "Gender by Category")

g1 = ggplot(employee_attrition) + geom_bar(aes(x=Gender, fill = MaritalStatus), position = "fill") + labs(y = "Percent")
g2 = ggplot(employee_attrition) + geom_bar(aes(x=MaritalStatus, fill = MaritalStatus), position = "fill") + labs(y = "Percent")
g3 = ggplot(employee_attrition) + geom_bar(aes(x=Education, fill = MaritalStatus), position = "fill") + labs(y = "Percent")
g4 = ggplot(employee_attrition) + geom_bar(aes(x=EducationField, fill = MaritalStatus), position = "fill") + labs(y = "Percent")
g5 = ggplot(employee_attrition) + geom_bar(aes(x=PerformanceRating, fill = MaritalStatus), position = "fill") + labs(y = "Percent")
g6 = ggplot(employee_attrition) + geom_bar(aes(x=Department, fill = MaritalStatus), position = "fill") + labs(y = "Percent")
g7 = ggplot(employee_attrition) + geom_bar(aes(x=JobLevel, fill = MaritalStatus), position = "fill") + labs(y = "Percent")
g8 = ggplot(employee_attrition) + geom_bar(aes(x=JobRole, fill = MaritalStatus), position = "fill") + labs(y = "Percent")
g9 = ggplot(employee_attrition) + geom_bar(aes(x=EnvironmentSatisfaction, fill = MaritalStatus), position = "fill") + labs(y = "Percent")
g10 = ggplot(employee_attrition) + geom_bar(aes(x=JobInvolvement, fill = MaritalStatus), position = "fill") + labs(y = "Percent")
g11 = ggplot(employee_attrition) + geom_bar(aes(x=JobSatisfaction, fill = MaritalStatus), position = "fill") + labs(y = "Percent")
g12 = ggplot(employee_attrition) + geom_bar(aes(x=RelationshipSatisfaction, fill = MaritalStatus), position = "fill") + labs(y = "Percent")
g13 = ggplot(employee_attrition) + geom_bar(aes(x=WorkLifeBalance, fill = MaritalStatus), position = "fill") + labs(y = "Percent")
g14 = ggplot(employee_attrition) + geom_bar(aes(x=BusinessTravel, fill = MaritalStatus), position = "fill") + labs(y = "Percent")

grid.arrange(g1, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, g14,  nrow=5, top = "MarialStatus by Category")

g1 = ggplot(employee_attrition) + geom_bar(aes(x=Gender, fill = Education), position = "fill") + labs(y = "Percent")
g2 = ggplot(employee_attrition) + geom_bar(aes(x=MaritalStatus, fill = Education), position = "fill") + labs(y = "Percent")
g3 = ggplot(employee_attrition) + geom_bar(aes(x=Education, fill = Education), position = "fill") + labs(y = "Percent")
g4 = ggplot(employee_attrition) + geom_bar(aes(x=EducationField, fill = Education), position = "fill") + labs(y = "Percent")
g5 = ggplot(employee_attrition) + geom_bar(aes(x=PerformanceRating, fill = Education), position = "fill") + labs(y = "Percent")
g6 = ggplot(employee_attrition) + geom_bar(aes(x=Department, fill = Education), position = "fill") + labs(y = "Percent")
g7 = ggplot(employee_attrition) + geom_bar(aes(x=JobLevel, fill = Education), position = "fill") + labs(y = "Percent")
g8 = ggplot(employee_attrition) + geom_bar(aes(x=JobRole, fill = Education), position = "fill") + labs(y = "Percent")
g9 = ggplot(employee_attrition) + geom_bar(aes(x=EnvironmentSatisfaction, fill = Education), position = "fill") + labs(y = "Percent")
g10 = ggplot(employee_attrition) + geom_bar(aes(x=JobInvolvement, fill = Education), position = "fill") + labs(y = "Percent")
g11 = ggplot(employee_attrition) + geom_bar(aes(x=JobSatisfaction, fill = Education), position = "fill") + labs(y = "Percent")
g12 = ggplot(employee_attrition) + geom_bar(aes(x=RelationshipSatisfaction, fill = Education), position = "fill") + labs(y = "Percent")
g13 = ggplot(employee_attrition) + geom_bar(aes(x=WorkLifeBalance, fill = Education), position = "fill") + labs(y = "Percent")
g14 = ggplot(employee_attrition) + geom_bar(aes(x=BusinessTravel, fill = Education), position = "fill") + labs(y = "Percent")

grid.arrange(g1, g2, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, g14,  nrow=5, top = "Education by Category")

g1 = ggplot(employee_attrition) + geom_bar(aes(x=Gender, fill = EducationField), position = "fill") + labs(y = "Percent")
g2 = ggplot(employee_attrition) + geom_bar(aes(x=MaritalStatus, fill = EducationField), position = "fill") + labs(y = "Percent")
g3 = ggplot(employee_attrition) + geom_bar(aes(x=Education, fill = EducationField), position = "fill") + labs(y = "Percent")
g4 = ggplot(employee_attrition) + geom_bar(aes(x=EducationField, fill = EducationField), position = "fill") + labs(y = "Percent")
g5 = ggplot(employee_attrition) + geom_bar(aes(x=PerformanceRating, fill = EducationField), position = "fill") + labs(y = "Percent")
g6 = ggplot(employee_attrition) + geom_bar(aes(x=Department, fill = EducationField), position = "fill") + labs(y = "Percent")
g7 = ggplot(employee_attrition) + geom_bar(aes(x=JobLevel, fill = EducationField), position = "fill") + labs(y = "Percent")
g8 = ggplot(employee_attrition) + geom_bar(aes(x=JobRole, fill = EducationField), position = "fill") + labs(y = "Percent")
g9 = ggplot(employee_attrition) + geom_bar(aes(x=EnvironmentSatisfaction, fill = EducationField), position = "fill") + labs(y = "Percent")
g10 = ggplot(employee_attrition) + geom_bar(aes(x=JobInvolvement, fill = EducationField), position = "fill") + labs(y = "Percent")
g11 = ggplot(employee_attrition) + geom_bar(aes(x=JobSatisfaction, fill = EducationField), position = "fill") + labs(y = "Percent")
g12 = ggplot(employee_attrition) + geom_bar(aes(x=RelationshipSatisfaction, fill = EducationField), position = "fill") + labs(y = "Percent")
g13 = ggplot(employee_attrition) + geom_bar(aes(x=WorkLifeBalance, fill = EducationField), position = "fill") + labs(y = "Percent")
g14 = ggplot(employee_attrition) + geom_bar(aes(x=BusinessTravel, fill = EducationField), position = "fill") + labs(y = "Percent")

grid.arrange(g1, g2, g3, g5, g6, g7, g8, g9, g10, g11, g12, g13, g14,  nrow=5, top = "EducationField by Category")

g1 = ggplot(employee_attrition) + geom_bar(aes(x=Gender, fill = PerformanceRating), position = "fill") + labs(y = "Percent")
g2 = ggplot(employee_attrition) + geom_bar(aes(x=MaritalStatus, fill = PerformanceRating), position = "fill") + labs(y = "Percent")
g3 = ggplot(employee_attrition) + geom_bar(aes(x=Education, fill = PerformanceRating), position = "fill") + labs(y = "Percent")
g4 = ggplot(employee_attrition) + geom_bar(aes(x=EducationField, fill = PerformanceRating), position = "fill") + labs(y = "Percent")
g5 = ggplot(employee_attrition) + geom_bar(aes(x=PerformanceRating, fill = PerformanceRating), position = "fill") + labs(y = "Percent")
g6 = ggplot(employee_attrition) + geom_bar(aes(x=Department, fill = PerformanceRating), position = "fill") + labs(y = "Percent")
g7 = ggplot(employee_attrition) + geom_bar(aes(x=JobLevel, fill = PerformanceRating), position = "fill") + labs(y = "Percent")
g8 = ggplot(employee_attrition) + geom_bar(aes(x=JobRole, fill = PerformanceRating), position = "fill") + labs(y = "Percent")
g9 = ggplot(employee_attrition) + geom_bar(aes(x=EnvironmentSatisfaction, fill = PerformanceRating), position = "fill") + labs(y = "Percent")
g10 = ggplot(employee_attrition) + geom_bar(aes(x=JobInvolvement, fill = PerformanceRating), position = "fill") + labs(y = "Percent")
g11 = ggplot(employee_attrition) + geom_bar(aes(x=JobSatisfaction, fill = PerformanceRating), position = "fill") + labs(y = "Percent")
g12 = ggplot(employee_attrition) + geom_bar(aes(x=RelationshipSatisfaction, fill = PerformanceRating), position = "fill") + labs(y = "Percent")
g13 = ggplot(employee_attrition) + geom_bar(aes(x=WorkLifeBalance, fill = PerformanceRating), position = "fill") + labs(y = "Percent")
g14 = ggplot(employee_attrition) + geom_bar(aes(x=BusinessTravel, fill = PerformanceRating), position = "fill") + labs(y = "Percent")

grid.arrange(g1, g2, g3, g4, g6, g7, g8, g9, g10, g11, g12, g13, g14,  nrow=5, top = "PerformanceRating by Category")

g1 = ggplot(employee_attrition) + geom_bar(aes(x=Gender, fill = Department), position = "fill") + labs(y = "Percent")
g2 = ggplot(employee_attrition) + geom_bar(aes(x=MaritalStatus, fill = Department), position = "fill") + labs(y = "Percent")
g3 = ggplot(employee_attrition) + geom_bar(aes(x=Education, fill = Department), position = "fill") + labs(y = "Percent")
g4 = ggplot(employee_attrition) + geom_bar(aes(x=EducationField, fill = Department), position = "fill") + labs(y = "Percent")
g5 = ggplot(employee_attrition) + geom_bar(aes(x=PerformanceRating, fill = Department), position = "fill") + labs(y = "Percent")
g6 = ggplot(employee_attrition) + geom_bar(aes(x=Department, fill = Department), position = "fill") + labs(y = "Percent")
g7 = ggplot(employee_attrition) + geom_bar(aes(x=JobLevel, fill = Department), position = "fill") + labs(y = "Percent")
g8 = ggplot(employee_attrition) + geom_bar(aes(x=JobRole, fill = Department), position = "fill") + labs(y = "Percent")
g9 = ggplot(employee_attrition) + geom_bar(aes(x=EnvironmentSatisfaction, fill = Department), position = "fill") + labs(y = "Percent")
g10 = ggplot(employee_attrition) + geom_bar(aes(x=JobInvolvement, fill = Department), position = "fill") + labs(y = "Percent")
g11 = ggplot(employee_attrition) + geom_bar(aes(x=JobSatisfaction, fill = Department), position = "fill") + labs(y = "Percent")
g12 = ggplot(employee_attrition) + geom_bar(aes(x=RelationshipSatisfaction, fill = Department), position = "fill") + labs(y = "Percent")
g13 = ggplot(employee_attrition) + geom_bar(aes(x=WorkLifeBalance, fill = Department), position = "fill") + labs(y = "Percent")
g14 = ggplot(employee_attrition) + geom_bar(aes(x=BusinessTravel, fill = Department), position = "fill") + labs(y = "Percent")

grid.arrange(g1, g2, g3, g4, g5, g7, g8, g9, g10, g11, g12, g13, g14,  nrow=5, top = "Department by Category")

g1 = ggplot(employee_attrition) + geom_bar(aes(x=Gender, fill = JobLevel), position = "fill") + labs(y = "Percent")
g2 = ggplot(employee_attrition) + geom_bar(aes(x=MaritalStatus, fill = JobLevel), position = "fill") + labs(y = "Percent")
g3 = ggplot(employee_attrition) + geom_bar(aes(x=Education, fill = JobLevel), position = "fill") + labs(y = "Percent")
g4 = ggplot(employee_attrition) + geom_bar(aes(x=EducationField, fill = JobLevel), position = "fill") + labs(y = "Percent")
g5 = ggplot(employee_attrition) + geom_bar(aes(x=PerformanceRating, fill = JobLevel), position = "fill") + labs(y = "Percent")
g6 = ggplot(employee_attrition) + geom_bar(aes(x=Department, fill = JobLevel), position = "fill") + labs(y = "Percent")
g7 = ggplot(employee_attrition) + geom_bar(aes(x=JobLevel, fill = JobLevel), position = "fill") + labs(y = "Percent")
g8 = ggplot(employee_attrition) + geom_bar(aes(x=JobRole, fill = JobLevel), position = "fill") + labs(y = "Percent")
g9 = ggplot(employee_attrition) + geom_bar(aes(x=EnvironmentSatisfaction, fill = JobLevel), position = "fill") + labs(y = "Percent")
g10 = ggplot(employee_attrition) + geom_bar(aes(x=JobInvolvement, fill = JobLevel), position = "fill") + labs(y = "Percent")
g11 = ggplot(employee_attrition) + geom_bar(aes(x=JobSatisfaction, fill = JobLevel), position = "fill") + labs(y = "Percent")
g12 = ggplot(employee_attrition) + geom_bar(aes(x=RelationshipSatisfaction, fill = JobLevel), position = "fill") + labs(y = "Percent")
g13 = ggplot(employee_attrition) + geom_bar(aes(x=WorkLifeBalance, fill = JobLevel), position = "fill") + labs(y = "Percent")
g14 = ggplot(employee_attrition) + geom_bar(aes(x=BusinessTravel, fill = JobLevel), position = "fill") + labs(y = "Percent")

grid.arrange(g1, g2, g3, g4, g5, g6, g8, g9, g10, g11, g12, g13, g14,  nrow=5, top = "JobLevel by Category")

g1 = ggplot(employee_attrition) + geom_bar(aes(x=Gender, fill = JobRole), position = "fill") + labs(y = "Percent")
g2 = ggplot(employee_attrition) + geom_bar(aes(x=MaritalStatus, fill = JobRole), position = "fill") + labs(y = "Percent")
g3 = ggplot(employee_attrition) + geom_bar(aes(x=Education, fill = JobRole), position = "fill") + labs(y = "Percent")
g4 = ggplot(employee_attrition) + geom_bar(aes(x=EducationField, fill = JobRole), position = "fill") + labs(y = "Percent")
g5 = ggplot(employee_attrition) + geom_bar(aes(x=PerformanceRating, fill = JobRole), position = "fill") + labs(y = "Percent")
g6 = ggplot(employee_attrition) + geom_bar(aes(x=Department, fill = JobRole), position = "fill") + labs(y = "Percent")
g7 = ggplot(employee_attrition) + geom_bar(aes(x=JobLevel, fill = JobRole), position = "fill") + labs(y = "Percent")
g8 = ggplot(employee_attrition) + geom_bar(aes(x=JobRole, fill = JobRole), position = "fill") + labs(y = "Percent")
g9 = ggplot(employee_attrition) + geom_bar(aes(x=EnvironmentSatisfaction, fill = JobRole), position = "fill") + labs(y = "Percent")
g10 = ggplot(employee_attrition) + geom_bar(aes(x=JobInvolvement, fill = JobRole), position = "fill") + labs(y = "Percent")
g11 = ggplot(employee_attrition) + geom_bar(aes(x=JobSatisfaction, fill = JobRole), position = "fill") + labs(y = "Percent")
g12 = ggplot(employee_attrition) + geom_bar(aes(x=RelationshipSatisfaction, fill = JobRole), position = "fill") + labs(y = "Percent")
g13 = ggplot(employee_attrition) + geom_bar(aes(x=WorkLifeBalance, fill = JobRole), position = "fill") + labs(y = "Percent")
g14 = ggplot(employee_attrition) + geom_bar(aes(x=BusinessTravel, fill = JobRole), position = "fill") + labs(y = "Percent")

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g9, g10, g11, g12, g13, g14,  nrow=5, top = "JobRole by Category")

g1 = ggplot(employee_attrition) + geom_bar(aes(x=Gender, fill = EnvironmentSatisfaction), position = "fill") + labs(y = "Percent")
g2 = ggplot(employee_attrition) + geom_bar(aes(x=MaritalStatus, fill = EnvironmentSatisfaction), position = "fill") + labs(y = "Percent")
g3 = ggplot(employee_attrition) + geom_bar(aes(x=Education, fill = EnvironmentSatisfaction), position = "fill") + labs(y = "Percent")
g4 = ggplot(employee_attrition) + geom_bar(aes(x=EducationField, fill = EnvironmentSatisfaction), position = "fill") + labs(y = "Percent")
g5 = ggplot(employee_attrition) + geom_bar(aes(x=PerformanceRating, fill = EnvironmentSatisfaction), position = "fill") + labs(y = "Percent")
g6 = ggplot(employee_attrition) + geom_bar(aes(x=Department, fill = EnvironmentSatisfaction), position = "fill") + labs(y = "Percent")
g7 = ggplot(employee_attrition) + geom_bar(aes(x=JobLevel, fill = EnvironmentSatisfaction), position = "fill") + labs(y = "Percent")
g8 = ggplot(employee_attrition) + geom_bar(aes(x=JobRole, fill = EnvironmentSatisfaction), position = "fill") + labs(y = "Percent")
g9 = ggplot(employee_attrition) + geom_bar(aes(x=EnvironmentSatisfaction, fill = EnvironmentSatisfaction), position = "fill") + labs(y = "Percent")
g10 = ggplot(employee_attrition) + geom_bar(aes(x=JobInvolvement, fill = EnvironmentSatisfaction), position = "fill") + labs(y = "Percent")
g11 = ggplot(employee_attrition) + geom_bar(aes(x=JobSatisfaction, fill = EnvironmentSatisfaction), position = "fill") + labs(y = "Percent")
g12 = ggplot(employee_attrition) + geom_bar(aes(x=RelationshipSatisfaction, fill = EnvironmentSatisfaction), position = "fill") + labs(y = "Percent")
g13 = ggplot(employee_attrition) + geom_bar(aes(x=WorkLifeBalance, fill = EnvironmentSatisfaction), position = "fill") + labs(y = "Percent")
g14 = ggplot(employee_attrition) + geom_bar(aes(x=BusinessTravel, fill = EnvironmentSatisfaction), position = "fill") + labs(y = "Percent")

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g10, g11, g12, g13, g14,  nrow=5, top = "EnvironmentSatisfaction by Category")

g1 = ggplot(employee_attrition) + geom_bar(aes(x=Gender, fill = JobInvolvement), position = "fill") + labs(y = "Percent")
g2 = ggplot(employee_attrition) + geom_bar(aes(x=MaritalStatus, fill = JobInvolvement), position = "fill") + labs(y = "Percent")
g3 = ggplot(employee_attrition) + geom_bar(aes(x=Education, fill = JobInvolvement), position = "fill") + labs(y = "Percent")
g4 = ggplot(employee_attrition) + geom_bar(aes(x=EducationField, fill = JobInvolvement), position = "fill") + labs(y = "Percent")
g5 = ggplot(employee_attrition) + geom_bar(aes(x=PerformanceRating, fill = JobInvolvement), position = "fill") + labs(y = "Percent")
g6 = ggplot(employee_attrition) + geom_bar(aes(x=Department, fill = JobInvolvement), position = "fill") + labs(y = "Percent")
g7 = ggplot(employee_attrition) + geom_bar(aes(x=JobLevel, fill = JobInvolvement), position = "fill") + labs(y = "Percent")
g8 = ggplot(employee_attrition) + geom_bar(aes(x=JobRole, fill = JobInvolvement), position = "fill") + labs(y = "Percent")
g9 = ggplot(employee_attrition) + geom_bar(aes(x=EnvironmentSatisfaction, fill = JobInvolvement), position = "fill") + labs(y = "Percent")
g10 = ggplot(employee_attrition) + geom_bar(aes(x=JobInvolvement, fill = JobInvolvement), position = "fill") + labs(y = "Percent")
g11 = ggplot(employee_attrition) + geom_bar(aes(x=JobSatisfaction, fill = JobInvolvement), position = "fill") + labs(y = "Percent")
g12 = ggplot(employee_attrition) + geom_bar(aes(x=RelationshipSatisfaction, fill = JobInvolvement), position = "fill") + labs(y = "Percent")
g13 = ggplot(employee_attrition) + geom_bar(aes(x=WorkLifeBalance, fill = JobInvolvement), position = "fill") + labs(y = "Percent")
g14 = ggplot(employee_attrition) + geom_bar(aes(x=BusinessTravel, fill = JobInvolvement), position = "fill") + labs(y = "Percent")

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g11, g12, g13, g14,  nrow=5, top = "JobInvolvement by Category")

g1 = ggplot(employee_attrition) + geom_bar(aes(x=Gender, fill = JobSatisfaction), position = "fill") + labs(y = "Percent")
g2 = ggplot(employee_attrition) + geom_bar(aes(x=MaritalStatus, fill = JobSatisfaction), position = "fill") + labs(y = "Percent")
g3 = ggplot(employee_attrition) + geom_bar(aes(x=Education, fill = JobSatisfaction), position = "fill") + labs(y = "Percent")
g4 = ggplot(employee_attrition) + geom_bar(aes(x=EducationField, fill = JobSatisfaction), position = "fill") + labs(y = "Percent")
g5 = ggplot(employee_attrition) + geom_bar(aes(x=PerformanceRating, fill = JobSatisfaction), position = "fill") + labs(y = "Percent")
g6 = ggplot(employee_attrition) + geom_bar(aes(x=Department, fill = JobSatisfaction), position = "fill") + labs(y = "Percent")
g7 = ggplot(employee_attrition) + geom_bar(aes(x=JobLevel, fill = JobSatisfaction), position = "fill") + labs(y = "Percent")
g8 = ggplot(employee_attrition) + geom_bar(aes(x=JobRole, fill = JobSatisfaction), position = "fill") + labs(y = "Percent")
g9 = ggplot(employee_attrition) + geom_bar(aes(x=EnvironmentSatisfaction, fill = JobSatisfaction), position = "fill") + labs(y = "Percent")
g10 = ggplot(employee_attrition) + geom_bar(aes(x=JobInvolvement, fill = JobSatisfaction), position = "fill") + labs(y = "Percent")
g11 = ggplot(employee_attrition) + geom_bar(aes(x=JobSatisfaction, fill = JobSatisfaction), position = "fill") + labs(y = "Percent")
g12 = ggplot(employee_attrition) + geom_bar(aes(x=RelationshipSatisfaction, fill = JobSatisfaction), position = "fill") + labs(y = "Percent")
g13 = ggplot(employee_attrition) + geom_bar(aes(x=WorkLifeBalance, fill = JobSatisfaction), position = "fill") + labs(y = "Percent")
g14 = ggplot(employee_attrition) + geom_bar(aes(x=BusinessTravel, fill = JobSatisfaction), position = "fill") + labs(y = "Percent")

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g12, g13, g14,  nrow=5, top = "JobSatisfaction by Category")

g1 = ggplot(employee_attrition) + geom_bar(aes(x=Gender, fill = RelationshipSatisfaction), position = "fill") + labs(y = "Percent")
g2 = ggplot(employee_attrition) + geom_bar(aes(x=MaritalStatus, fill = RelationshipSatisfaction), position = "fill") + labs(y = "Percent")
g3 = ggplot(employee_attrition) + geom_bar(aes(x=Education, fill = RelationshipSatisfaction), position = "fill") + labs(y = "Percent")
g4 = ggplot(employee_attrition) + geom_bar(aes(x=EducationField, fill = RelationshipSatisfaction), position = "fill") + labs(y = "Percent")
g5 = ggplot(employee_attrition) + geom_bar(aes(x=PerformanceRating, fill = RelationshipSatisfaction), position = "fill") + labs(y = "Percent")
g6 = ggplot(employee_attrition) + geom_bar(aes(x=Department, fill = RelationshipSatisfaction), position = "fill") + labs(y = "Percent")
g7 = ggplot(employee_attrition) + geom_bar(aes(x=JobLevel, fill = RelationshipSatisfaction), position = "fill") + labs(y = "Percent")
g8 = ggplot(employee_attrition) + geom_bar(aes(x=JobRole, fill = RelationshipSatisfaction), position = "fill") + labs(y = "Percent")
g9 = ggplot(employee_attrition) + geom_bar(aes(x=EnvironmentSatisfaction, fill = RelationshipSatisfaction), position = "fill") + labs(y = "Percent")
g10 = ggplot(employee_attrition) + geom_bar(aes(x=JobInvolvement, fill = RelationshipSatisfaction), position = "fill") + labs(y = "Percent")
g11 = ggplot(employee_attrition) + geom_bar(aes(x=JobSatisfaction, fill = RelationshipSatisfaction), position = "fill") + labs(y = "Percent")
g12 = ggplot(employee_attrition) + geom_bar(aes(x=RelationshipSatisfaction, fill = RelationshipSatisfaction), position = "fill") + labs(y = "Percent")
g13 = ggplot(employee_attrition) + geom_bar(aes(x=WorkLifeBalance, fill = RelationshipSatisfaction), position = "fill") + labs(y = "Percent")
g14 = ggplot(employee_attrition) + geom_bar(aes(x=BusinessTravel, fill = RelationshipSatisfaction), position = "fill") + labs(y = "Percent")

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g13, g14,  nrow=5, top = "RelationshipSatisfaction by Category")

g1 = ggplot(employee_attrition) + geom_bar(aes(x=Gender, fill = WorkLifeBalance), position = "fill") + labs(y = "Percent")
g2 = ggplot(employee_attrition) + geom_bar(aes(x=MaritalStatus, fill = WorkLifeBalance), position = "fill") + labs(y = "Percent")
g3 = ggplot(employee_attrition) + geom_bar(aes(x=Education, fill = WorkLifeBalance), position = "fill") + labs(y = "Percent")
g4 = ggplot(employee_attrition) + geom_bar(aes(x=EducationField, fill = WorkLifeBalance), position = "fill") + labs(y = "Percent")
g5 = ggplot(employee_attrition) + geom_bar(aes(x=PerformanceRating, fill = WorkLifeBalance), position = "fill") + labs(y = "Percent")
g6 = ggplot(employee_attrition) + geom_bar(aes(x=Department, fill = WorkLifeBalance), position = "fill") + labs(y = "Percent")
g7 = ggplot(employee_attrition) + geom_bar(aes(x=JobLevel, fill = WorkLifeBalance), position = "fill") + labs(y = "Percent")
g8 = ggplot(employee_attrition) + geom_bar(aes(x=JobRole, fill = WorkLifeBalance), position = "fill") + labs(y = "Percent")
g9 = ggplot(employee_attrition) + geom_bar(aes(x=EnvironmentSatisfaction, fill = WorkLifeBalance), position = "fill") + labs(y = "Percent")
g10 = ggplot(employee_attrition) + geom_bar(aes(x=JobInvolvement, fill = WorkLifeBalance), position = "fill") + labs(y = "Percent")
g11 = ggplot(employee_attrition) + geom_bar(aes(x=JobSatisfaction, fill = WorkLifeBalance), position = "fill") + labs(y = "Percent")
g12 = ggplot(employee_attrition) + geom_bar(aes(x=RelationshipSatisfaction, fill = WorkLifeBalance), position = "fill") + labs(y = "Percent")
g13 = ggplot(employee_attrition) + geom_bar(aes(x=WorkLifeBalance, fill = WorkLifeBalance), position = "fill") + labs(y = "Percent")
g14 = ggplot(employee_attrition) + geom_bar(aes(x=BusinessTravel, fill = WorkLifeBalance), position = "fill") + labs(y = "Percent")

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g14,  nrow=5, top = "WorkLifeBalance by Category")

g1 = ggplot(employee_attrition) + geom_bar(aes(x=Gender, fill = BusinessTravel), position = "fill") + labs(y = "Percent")
g2 = ggplot(employee_attrition) + geom_bar(aes(x=MaritalStatus, fill = BusinessTravel), position = "fill") + labs(y = "Percent")
g3 = ggplot(employee_attrition) + geom_bar(aes(x=Education, fill = BusinessTravel), position = "fill") + labs(y = "Percent")
g4 = ggplot(employee_attrition) + geom_bar(aes(x=EducationField, fill = BusinessTravel), position = "fill") + labs(y = "Percent")
g5 = ggplot(employee_attrition) + geom_bar(aes(x=PerformanceRating, fill = BusinessTravel), position = "fill") + labs(y = "Percent")
g6 = ggplot(employee_attrition) + geom_bar(aes(x=Department, fill = BusinessTravel), position = "fill") + labs(y = "Percent")
g7 = ggplot(employee_attrition) + geom_bar(aes(x=JobLevel, fill = BusinessTravel), position = "fill") + labs(y = "Percent")
g8 = ggplot(employee_attrition) + geom_bar(aes(x=JobRole, fill = BusinessTravel), position = "fill") + labs(y = "Percent")
g9 = ggplot(employee_attrition) + geom_bar(aes(x=EnvironmentSatisfaction, fill = BusinessTravel), position = "fill") + labs(y = "Percent")
g10 = ggplot(employee_attrition) + geom_bar(aes(x=JobInvolvement, fill = BusinessTravel), position = "fill") + labs(y = "Percent")
g11 = ggplot(employee_attrition) + geom_bar(aes(x=JobSatisfaction, fill = BusinessTravel), position = "fill") + labs(y = "Percent")
g12 = ggplot(employee_attrition) + geom_bar(aes(x=RelationshipSatisfaction, fill = BusinessTravel), position = "fill") + labs(y = "Percent")
g13 = ggplot(employee_attrition) + geom_bar(aes(x=WorkLifeBalance, fill = BusinessTravel), position = "fill") + labs(y = "Percent")
g14 = ggplot(employee_attrition) + geom_bar(aes(x=BusinessTravel, fill = BusinessTravel), position = "fill") + labs(y = "Percent")

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13,  nrow=5, top = "BusinessTravel by Category")

#########################################################################################################################
#                                                                                                                       #
#  Analytical Modeling                                                                                                  #
#                                                                                                                       #
#########################################################################################################################
# Summarize interactions of outcome attribute with other numeric attributes
#
g1 = ggplot(employee_attrition) + geom_boxplot(aes(x= Attrition, y=Age)) 
g2 = ggplot(employee_attrition) + geom_boxplot(aes(x= Attrition, y=DistanceFromHome)) 
g3 = ggplot(employee_attrition) + geom_boxplot(aes(x= Attrition, y=NumCompaniesWorked)) 
g4 = ggplot(employee_attrition) + geom_boxplot(aes(x= Attrition, y=MonthlyIncome)) 
g5 = ggplot(employee_attrition) + geom_boxplot(aes(x= Attrition, y=TrainingTimesLastYear)) 
g6 = ggplot(employee_attrition) + geom_boxplot(aes(x= Attrition, y=PercentSalaryHike)) 
g7 = ggplot(employee_attrition) + geom_boxplot(aes(x= Attrition, y=YearsAtCompany)) 
g8 = ggplot(employee_attrition) + geom_boxplot(aes(x= Attrition, y=YearsInCurrentRole)) 
g9 = ggplot(employee_attrition) + geom_boxplot(aes(x= Attrition, y=YearsSinceLastPromotion)) 
g10 = ggplot(employee_attrition) + geom_boxplot(aes(x= Attrition, y=YearsWithCurrManager)) 
g11 = ggplot(employee_attrition) + geom_boxplot(aes(x= Attrition, y=TotalWorkingYears)) 

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, nrow=4)

attrition_cases = employee_attrition %>% filter(Attrition == "Yes")
non_attrition_cases = employee_attrition %>% filter(Attrition == "No")

t.test(Age ~ Attrition, data = employee_attrition)$conf.int
t.test(Age ~ Attrition, data = employee_attrition)$p.value
mean(attrition_cases$Age)
mean(non_attrition_cases$Age)

t.test(DistanceFromHome ~ Attrition, data = employee_attrition)$conf.int
t.test(DistanceFromHome ~ Attrition, data = employee_attrition)$p.value
mean(attrition_cases$DistanceFromHome)
mean(non_attrition_cases$DistanceFromHome)

t.test(NumCompaniesWorked ~ Attrition, data = employee_attrition)$conf.int
t.test(NumCompaniesWorked ~ Attrition, data = employee_attrition)$p.value
mean(attrition_cases$NumCompaniesWorked)
mean(non_attrition_cases$NumCompaniesWorked)

t.test(MonthlyIncome ~ Attrition, data = employee_attrition)$conf.int
t.test(DistanceFromHome ~ Attrition, data = employee_attrition)$p.value
mean(attrition_cases$MonthlyIncome)
mean(non_attrition_cases$MonthlyIncome)

t.test(TrainingTimesLastYear ~ Attrition, data = employee_attrition)$conf.int
t.test(TrainingTimesLastYear ~ Attrition, data = employee_attrition)$p.value
mean(attrition_cases$TrainingTimesLastYear)
mean(non_attrition_cases$TrainingTimesLastYear)

t.test(PercentSalaryHike ~ Attrition, data = employee_attrition)$conf.int
t.test(PercentSalaryHike ~ Attrition, data = employee_attrition)$p.value
mean(attrition_cases$PercentSalaryHike)
mean(non_attrition_cases$PercentSalaryHike)

t.test(YearsAtCompany ~ Attrition, data = employee_attrition)$conf.int
t.test(YearsAtCompany ~ Attrition, data = employee_attrition)$p.value
mean(attrition_cases$YearsAtCompany)
mean(non_attrition_cases$YearsAtCompany)

t.test(YearsInCurrentRole ~ Attrition, data = employee_attrition)$conf.int
t.test(YearsInCurrentRole ~ Attrition, data = employee_attrition)$p.value
mean(attrition_cases$YearsInCurrentRole)
mean(non_attrition_cases$YearsInCurrentRole)

t.test(YearsSinceLastPromotion ~ Attrition, data = employee_attrition)$conf.int
t.test(YearsSinceLastPromotion ~ Attrition, data = employee_attrition)$p.value
mean(attrition_cases$YearsSinceLastPromotion)
mean(non_attrition_cases$YearsSinceLastPromotion)

t.test(YearsWithCurrManager ~ Attrition, data = employee_attrition)$conf.int
t.test(YearsWithCurrManager ~ Attrition, data = employee_attrition)$p.value
mean(attrition_cases$YearsWithCurrManager)
mean(non_attrition_cases$YearsWithCurrManager)

t.test(TotalWorkingYears ~ Attrition, data = employee_attrition)$conf.int
t.test(TotalWorkingYears ~ Attrition, data = employee_attrition)$p.value
mean(attrition_cases$TotalWorkingYears)
mean(non_attrition_cases$TotalWorkingYears)
#
# Summarize interactions of outcome attribute with other cagegorical attributes
#
g1 = ggplot(employee_attrition) + geom_bar(aes(x=Gender, fill = Attrition), position = "fill") + labs(y = "Percent")
g2 = ggplot(employee_attrition) + geom_bar(aes(x=MaritalStatus, fill = Attrition), position = "fill") + labs(y = "Percent")
g3 = ggplot(employee_attrition) + geom_bar(aes(x=Education, fill = Attrition), position = "fill") + labs(y = "Percent")
g4 = ggplot(employee_attrition) + geom_bar(aes(x=EducationField, fill = Attrition), position = "fill") + labs(y = "Percent")
g5 = ggplot(employee_attrition) + geom_bar(aes(x=PerformanceRating, fill = Attrition), position = "fill") + labs(y = "Percent")
g6 = ggplot(employee_attrition) + geom_bar(aes(x=Department, fill = Attrition), position = "fill") + labs(y = "Percent")
g7 = ggplot(employee_attrition) + geom_bar(aes(x=JobLevel, fill = Attrition), position = "fill") + labs(y = "Percent")
g8 = ggplot(employee_attrition) + geom_bar(aes(x=JobRole, fill = Attrition), position = "fill") + labs(y = "Percent")
g9 = ggplot(employee_attrition) + geom_bar(aes(x=EnvironmentSatisfaction, fill = Attrition), position = "fill") + labs(y = "Percent")
g10 = ggplot(employee_attrition) + geom_bar(aes(x=JobInvolvement, fill = Attrition), position = "fill") + labs(y = "Percent")
g11 = ggplot(employee_attrition) + geom_bar(aes(x=JobSatisfaction, fill = Attrition), position = "fill") + labs(y = "Percent")
g12 = ggplot(employee_attrition) + geom_bar(aes(x=RelationshipSatisfaction, fill = Attrition), position = "fill") + labs(y = "Percent")
g13 = ggplot(employee_attrition) + geom_bar(aes(x=WorkLifeBalance, fill = Attrition), position = "fill") + labs(y = "Percent")
g14 = ggplot(employee_attrition) + geom_bar(aes(x=BusinessTravel, fill = Attrition), position = "fill") + labs(y = "Percent")

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, g14, nrow=5)

chisq.test(table(employee_attrition$Attrition, employee_attrition$Gender))
chisq.test(table(employee_attrition$Attrition, employee_attrition$MaritalStatus))
chisq.test(table(employee_attrition$Attrition, employee_attrition$Education))
chisq.test(table(employee_attrition$Attrition, employee_attrition$EducationField))
chisq.test(table(employee_attrition$Attrition, employee_attrition$PerformanceRating))
chisq.test(table(employee_attrition$Attrition, employee_attrition$Department))
chisq.test(table(employee_attrition$Attrition, employee_attrition$JobLevel))
chisq.test(table(employee_attrition$Attrition, employee_attrition$JobRole))
chisq.test(table(employee_attrition$Attrition, employee_attrition$EnvironmentSatisfaction))
chisq.test(table(employee_attrition$Attrition, employee_attrition$JobInvolvement))
chisq.test(table(employee_attrition$Attrition, employee_attrition$JobSatisfaction))
chisq.test(table(employee_attrition$Attrition, employee_attrition$RelationshipSatisfaction))
chisq.test(table(employee_attrition$Attrition, employee_attrition$WorkLifeBalance))
chisq.test(table(employee_attrition$Attrition, employee_attrition$BusinessTravel))

#
#  Look at full model using initial analytical technique
#
library(rpart)
library(rpart.plot)
full_tree = rpart(Attrition~., data = employee_attrition, method = "class")
rpart.plot(full_tree, tweak = 2, type = 5)

#
#  Generate decision trees for each job role
#
healthrep_employee_attrition = employee_attrition %>% filter(JobRole ==  "HealthRep")
hr_employee_attrition = employee_attrition %>% filter(JobRole ==  "HR")
labtech_employee_attrition = employee_attrition %>% filter(JobRole ==  "LabTech")
mgr_employee_attrition = employee_attrition %>% filter(JobRole ==  "Mgr")
mfgdir_employee_attrition = employee_attrition %>% filter(JobRole ==  "MfgDir")
researchdir_employee_attrition = employee_attrition %>% filter(JobRole ==  "ResearchDir")
researchsci_employee_attrition = employee_attrition %>% filter(JobRole ==  "ResearchSci")
salesexec_employee_attrition = employee_attrition %>% filter(JobRole ==  "SalesExec")
salesrep_employee_attrition = employee_attrition %>% filter(JobRole ==  "SalesRep")

rpart.plot(rpart(Attrition~., data = healthrep_employee_attrition, method = "class"), tweak = 2, type = 5, main = "HealthRep Decision Tree")
rpart.plot(rpart(Attrition~., data = hr_employee_attrition, method = "class"), tweak = 2, type = 5, main = "HR Decision Tree")
rpart.plot(rpart(Attrition~., data = labtech_employee_attrition, method = "class"), tweak = 2, type = 5, main = "LabTech Decision Tree")
rpart.plot(rpart(Attrition~., data = mgr_employee_attrition, method = "class"), tweak = 2, type = 5, main = "Mgr Decision Tree")
rpart.plot(rpart(Attrition~., data = mfgdir_employee_attrition, method = "class"), tweak = 2, type = 5, main = "MgfDir Decision Tree")
rpart.plot(rpart(Attrition~., data = researchdir_employee_attrition, method = "class"), tweak = 2, type = 5, main = "ResearchDir Decision Tree")
rpart.plot(rpart(Attrition~., data = researchsci_employee_attrition, method = "class"), tweak = 2, type = 5, main = "ResearchScienctist Decision Tree")
rpart.plot(rpart(Attrition~., data = salesexec_employee_attrition, method = "class"), tweak = 2, type = 5, main = "SalesExec Decision Tree")
rpart.plot(rpart(Attrition~., data = salesrep_employee_attrition, method = "class"), tweak = 2, type = 5, main = "SalesRep Decision Tree")
#
#  Develop clusters of employees who have left
#
employee_departed = employee_attrition %>% filter(Attrition == "Yes")
km_emp_departed = kmeans(scale(employee_departed %>% 
                                 select(Age, DistanceFromHome, MonthlyIncome, NumCompaniesWorked, TotalWorkingYears, YearsAtCompany, 
                                        YearsInCurrentRole, YearsSinceLastPromotion, YearsWithCurrManager)), 3)


set.seed(1)
km_emp = kmeans(scale(employee_attrition %>% 
                  select(Age, DistanceFromHome, MonthlyIncome, NumCompaniesWorked, TotalWorkingYears, YearsAtCompany, 
                         YearsInCurrentRole, YearsSinceLastPromotion, YearsWithCurrManager)), 3)

employee_attrition = employee_attrition %>% mutate(demog_cluster = km_emp$cluster)

cluster_1 = employee_attrition %>% filter(demog_cluster == 1)
cluster_2 = employee_attrition %>% filter(demog_cluster == 2)
cluster_3 = employee_attrition %>% filter(demog_cluster == 3)

summarize_numeric(cluster_1)
summarize_numeric(cluster_2)
summarize_numeric(cluster_3)
#
#  Hypothesize employee groupings that may have different factors influencing attrition
#
employee_attrition = employee_attrition %>% mutate(AgeBin = ifelse(Age<40, "Younger", "Older"))
employee_attrition = employee_attrition %>% mutate(SalaryBin = ifelse(MonthlyIncome<8000, "Low Paid", "High Paid"))
employee_attrition = employee_attrition %>% mutate(EducationBin = ifelse(Education == "Master" | Education == "Doctor", "AdvancedDegree", "Other"))

employee_attrition = employee_attrition %>% mutate(EmployeeGroup = as.factor(
  case_when(
    AgeBin == "Younger" & SalaryBin == "High Paid" & EducationBin == "AdvancedDegree" ~ 'Younger/Higher Paid/Advanced Degrees',
    AgeBin == "Younger" & SalaryBin == "High Paid" & EducationBin == "Other" ~ 'Younger/Higher Paid/No Advanced Degrees',
    AgeBin == "Younger" & SalaryBin == "Low Paid" & EducationBin == "AdvancedDegree" ~ 'Younger/Lower Paid/Advanced Degrees',
    AgeBin == "Younger" & SalaryBin == "Low Paid" & EducationBin == "Other" ~ 'Younger/Lower Paid/No Advanced Degrees',
    AgeBin == "Older" & SalaryBin == "High Paid" & EducationBin == "AdvancedDegree" ~ 'Older/Higher Paid/Advanced Degrees',
    AgeBin == "Older" & SalaryBin == "High Paid" & EducationBin == "Other" ~ 'Older/Higher Paid/No Advanced Degrees',
    AgeBin == "Older" & SalaryBin == "Low Paid" & EducationBin == "AdvancedDegree" ~ 'Older/Lower Paid/Advanced Degrees',
    AgeBin == "Older" & SalaryBin == "Low Paid" & EducationBin == "Other" ~ 'Older/Lower Paid/No Advanced Degrees'
  )
)
)
ggplot(employee_attrition) + geom_bar(aes(y=EmployeeGroup, fill = Attrition))

employee_young_high_advanced = employee_attrition %>% filter(AgeBin == "Younger" & SalaryBin == "High Paid" & EducationBin == "AdvancedDegree")
employee_young_high_other = employee_attrition %>% filter(AgeBin == "Younger" & SalaryBin == "High Paid" & EducationBin == "Other")
employee_young_low_advanced = employee_attrition %>% filter(AgeBin == "Younger" & SalaryBin == "Low Paid" & EducationBin == "AdvancedDegree")
employee_young_low_other = employee_attrition %>% filter(AgeBin == "Younger" & SalaryBin == "Low Paid" & EducationBin == "Other")
employee_older_high_advanced  = employee_attrition %>% filter(AgeBin == "Older" & SalaryBin == "High Paid" & EducationBin == "AdvancedDegree")
employee_older_high_other = employee_attrition %>% filter(AgeBin == "Older" & SalaryBin == "High Paid" & EducationBin == "Other")
employee_older_low_advanced  = employee_attrition %>% filter(AgeBin == "Older" & SalaryBin == "Low Paid" & EducationBin == "AdvancedDegree")
employee_older_low_other = employee_attrition %>% filter(AgeBin == "Older" & SalaryBin == "Low Paid" & EducationBin == "Other")
#
#  Decision trees by employee groups
#
rpart.plot(rpart(Attrition~., data = employee_young_high_advanced, method = "class"), tweak = 1, type = 5, main = "Younger, High Paid, Advanced Degrees: Decision Tree")
rpart.plot(rpart(Attrition~., data = employee_young_high_other, method = "class"), tweak = 2, type = 5, main = "Younger, High Paid, Non-Advanced Degrees: Decision Tree")
rpart.plot(rpart(Attrition~., data = employee_young_low_advanced, method = "class"), tweak = 2, type = 5, main = "Younger, Lower Paid, Advanced Degrees: Decision Tree")
rpart.plot(rpart(Attrition~., data = employee_young_low_other, method = "class"), tweak = 2, type = 5, main = "Younger, Lower Paid, Non-Advanced Degrees: Decision Tree")
rpart.plot(rpart(Attrition~., data = employee_older_high_advanced, method = "class"), tweak = 2, type = 5, main = "Older, High Paid, Advanced Degrees: Decision Tree")
rpart.plot(rpart(Attrition~., data = employee_older_high_other, method = "class"), tweak = 2, type = 5, main = "Older, High Paid, Non-Advanced Degrees: Decision Tree")
rpart.plot(rpart(Attrition~., data = employee_older_low_advanced, method = "class"), tweak = 2, type = 5, main = "Older, Lower Paid, Advanced Degrees: Decision Tree")
rpart.plot(rpart(Attrition~., data = employee_older_low_other, method = "class"), tweak = 2, type = 5, main = "Older, Lower Paid, Non-Advanced Degrees: Decision Tree")


