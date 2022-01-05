#Loading Data:
Hr<-read.csv("D://edureka//R for data science//CertficationProject/HRM.csv")
View(Hr)

#Exploratory Data Analysis:
table(Hr$satisfaction_level)
table(Hr$number_project)
table(Hr$average_montly_hours)
table(Hr$time_spend_company)
table(Hr$Work_accident)
table(Hr$left)
table(Hr$promotion_last_5years)
table(Hr$department)

library("dplyr")
Hr <- Hr %>% select(satisfaction_level,last_evaluation,number_project,,average_montly_hours,time_spend_company,Work_accident,promotion_last_5years,department,salary,left)
Hr

Hr$left <- as.numeric(Hr$left)
colnames(Hr)
Hr$number_project <- as.numeric(Hr$number_project) 
Hr$average_montly_hours <- as.numeric(Hr$average_montly_hours)
Hr$time_spend_company <- as.numeric(Hr$time_spend_company)
Hr$Work_accident <- as.numeric(Hr$Work_accident)
Hr$promotion_last_5years <- as.numeric(Hr$promotion_last_5years)
Hr$department <- as.numeric(Hr$department)
Hr$salary <- as.numeric(Hr$salary)

library("corrplot")
Hr <- Hr[, c(1:10)]
head(Hr)

sapply(Hr , class)

#correlation values of the attributes of our data
Hr.cor = cor(Hr)
corrplot(Hr.cor)

res <- cor(Hr)
round(res, 2)

# create histograms for each attribute
  for(i in 10) {
  hist(Hr[,i], main=names(Hr)[i])
}

#Evaluating the values of each attributes for both left and non-left employees:

table(Hr$department,Hr$left)

#Analyzing  the  department  wise  turnouts  and  finding  out  the  percentage  of  
#employees leaving from each department:


#Percentage of employees leaving from accounting department:
turnout_accounting_total <- 767
turnout_accounting_left<- 204
turnout_accounting_percentage_left<-(turnout_accounting_left/turnout_accounting_total)*100
turnout_accounting_percentage_left
#turnout_accounting_percentage_left_ = 26.59713%

#Percentage of employees leaving from hr department:
turnout_hr_total <- 739
turnout_hr_left<- 215
turnout_hr_percentage_left<-(turnout_hr_left/turnout_hr_total)*100
turnout_hr_percentage_left
#turnout_hr_percentage_left = 29.09337%

#Percentage of employees leaving from IT department:
turnout_IT_total <- 1127
turnout_IT_left<- 273
turnout_IT_percentage_left<-(turnout_IT_left/turnout_IT_total)*100
turnout_IT_percentage_left
#turnout_IT_percentage_left = 24.2236%

#Percentage of employees leaving from management department:
turnout_management_total <- 630
turnout_management_left<- 91
turnout_management_percentage_left<-(turnout_management_left/turnout_management_total)*100
turnout_management_percentage_left
#turnout_management_percentage_left = 14.44444%

#Percentage of employees leaving from marketing department:
turnout_marketing_total <- 858
turnout_marketing_left<- 203
turnout_marketing_percentage_left<-(turnout_marketing_left/turnout_marketing_total)*100
turnout_marketing_percentage_left
#turnout_marketing_percentage_left = 23.65967%

#Percentage of employees leaving from product_mng department:
turnout_product_mng_total <- 902
turnout_product_mng_left<- 198
turnout_product_mng_percentage_left<-(turnout_product_mng_left/turnout_product_mng_total)*100
turnout_product_mng_percentage_left
#turnout_product_mng_percentage_left = 21.95122%

#Percentage of employees leaving from RandD department:
turnout_RandD_total <- 787
turnout_RandD_left<- 121
turnout_RandD_percentage_left<-(turnout_RandD_left/turnout_RandD_total)*100
turnout_RandD_percentage_left
#turnout_RandD_percentage_left = 15.37484%

#Percentage of employees leaving from sales department:
turnout_sales_total <- 4140
turnout_sales_left<- 1014
turnout_sales_percentage_left<-(turnout_sales_left/turnout_sales_total)*100
turnout_sales_percentage_left
#turnout_sales_percentage_left = 24.4927%

#Percentage of employees leaving from support department:
turnout_support_total <- 2229
turnout_support_left<- 555
turnout_support_percentage_left<-(turnout_support_left/turnout_support_total)*100
turnout_support_percentage_left
#turnout_support_percentage_left = 24.89906%

#Percentage of employees leaving from technical department:
turnout_technical_total <- 2720
turnout_technical_left<- 697
turnout_technical_percentage_left<-(turnout_technical_left/turnout_technical_total)*100
turnout_technical_percentage_left
#turnout_technical_percentage_left = 25.625%

#Building a classification model to forecast what are the attributes of people who leave the company:
Hr$left <- factor(Hr$left , levels =c(0,1))
sapply(Hr , class)

##########################Decision Tree#########################################

# Splitting the dataset into the Training set and Test set
library(caTools)

set.seed(123)
split = sample.split(Hr$left, SplitRatio = 0.7)

training_set = subset(Hr, split == TRUE)
test_set = subset(Hr, split == FALSE)

# Feature Scaling
training_set[-10] = scale(training_set[-10])
training_set
test_set[-10] = scale(test_set[-10])
test_set
View(Hr)

library(e1071)
library(rpart)
#Building a decision tree model:
dt_model <- rpart(training_set$left~.,data = training_set)
summary(dt_model)
plot(dt_model ,margin = 0.1)
text(dt_model, use.n = T,pretty = T,cex = 0.8)

#Predicting from the model:
predvalues <- predict(dt_model , newdata = test_set ,type = "class")
predvalues

#Evaluating:
library(caret)
confusionMatrix(table(predvalues , test_set$left))
class(predvalues)
class(test_set$left)
table(predvalues)

###############################Random Forest####################################
library(randomForest)
str(Hr)

#Building a ramdom forest model:
rf_model<- randomForest(training_set$left~.,data = training_set , ntree = 100)
rf_model
plot(rf_model)

#Predicting from the model:
rf_predvalues <- predict(rf_model , newdata = test_set ,type = "class")
rf_predvalues

#Evaluating:
confusionMatrix(table(rf_predvalues , test_set$left))
class(predvalues)
class(test_set$left)
table(predvalues)

#Variable Importance:
varImp(rf_model)
varImpPlot(rf_model,sort = T,type = 2)
varUsed(rf_model)

##############################Naive Bayes#######################################
#Building a Naive Bayes model:
nb_model <- naiveBayes(training_set$left~.,data = training_set)
nb_model

#Predicting from the model:
nb_predvalues <- predict(nb_model ,newdata =test_set ,type = "class")
nb_predvalues

#Evaluating:
confusionMatrix(table(nb_predvalues , test_set$left))
class(nb_predvalues)
class(test_set$left)
table(nb_predvalues)
#########################Support Vector Machine#################################

svm_model = svm(formula = left ~ .,
                data = training_set,
                type = 'C-classification',
                kernel = 'linear',
                cost =0.1)
svm_model

# Predicting the Test set results
y_pred = predict(svm_model, newdata = test_set)
y_pred

#Evaluating:
confusionMatrix(table(y_pred ,test_set$left))
class(y_pred)
class(test_set$left)
table(y_pred)

################################Conclusion######################################
#In the above dataset i.e from Human Resource management we have concluded the following:

#1. The first step was EDA and we checked the correlation between the matrix and following were the result:
# Percentage of people left from each department:
# Accounting <- 26.59713 %
# hr = 29.09337 %
# IT = 24.2236 % 
# management = 14.44444 %
# marketing = 23.65967 %
# product_mng = 21.95122 %
# RandD = 15.37484 %
# sales = 24.4927 %
# support = 24.89906 %
# technical = 25.625%
# The highest percentage of people that left is from HR department.

#2.The correlation Matrix between the variables: 
# satisfaction_level and number_of_project is negatively correlated with the correlation of -0.14
# satisfaction_level and time_spend_company is negatively correlated with the correlation of -0.10
# satisfaction_level and work_accident is correlated with the correlation of 0.06
# satisfaction_level and left is negatively correlated with the correlation of -0.39

# last evalution and satisfaction_level is positively correlated with the correlation of 0.11
# last evalution and number_of_project is positively correlated with the correlation of 0.35
# last evalution and average_monthly_hours is positively correlated with the correlation of 0.34
# last evalution and time_spend_company is positively correlated with the correlation of 0.13

# number_of_project and average_monthly_hours is positively correlated with the correlation of 0.42
# number_of_project and time_spend_company is positively correlated with the correlation of 0.20 

# average_monthly_hours and time_spend_company is positively correlated with the correlation of 0.13
# average_monthly_hours and left is correlated with the correlation of 0.07

# time_spend_company and promotion_last_5years is correlated with the correlation of 0.07
# time_spend_company and left is correlated with the correlation of 0.14

# work_accident and left is negatively correlated with the correlation of -0.15.

# promotion_last_5years and left is negatively correlated with the correlation of -0.06

################################################################################
#2.#Building a classification model to forecast what are the attributes of people who leave the company:   
#a. Decision Tree:
  # this model was build with the accuracy of 97.11 %
  # In which the model out of total 4499 observations,predicted 3389 True positves,91 False Positve,39 False Negative,980 True Negative.
  # Variable Improtance was : 34 - satisfaction_level , 17 - number_of_projects, 17 - average_monthly_hours,
  #                           17 - last_evaluation, 13 - time_spend_company.

#b. Random_forest:
  # this model was build with the accuracy of 98.93 %
  # In which the model out of total 4499 observations,predicted 3423 True positves,43 False Positve,5 False Negative,1028 True Negative.
  # Variable Improtance was : Overall
  #                           satisfaction_level    1285.814281
  #                           last_evaluation        453.072783
  #                           number_project         680.861009
  #                           average_montly_hours   578.985322
  #                           time_spend_company     681.211911
  #                           Work_accident           26.785521
  #                           promotion_last_5years    3.313381
  #                           department              45.382875
  #                           salary                  28.389578


#c. Naive Bayes:
  # this model was build with the accuracy of 79.46 %
  # In which the model out of total 4499 observations,predicted 2805 True positves,301 False Positve,623 False Negative,770 True Negative.

#d. Support Vector Machine:
  # this model was build with the accuracy of 77.66 %
  # In which the model out of total 4499 observations,predicted 3240 True positves,817 False Positve,188 False Negative,254 True Negative.  

################################################################################
#Conclusion : We will build Random Forest classifier for this dataset because it has the highest accuracy,
#             i.e 98.93 % and It has the lowest False Negative values i.e out of 4499 obeservations,
#             the model has predicted only 5 False Negative values.  