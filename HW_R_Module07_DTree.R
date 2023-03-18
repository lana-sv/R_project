
#R for Data Science - class August-Sept 2022 with Prateek
#Homework
#Module 07, Assignment 01 - 08:  Dtree for placement dataset, cc, pharma.

#Module 07, Assignment 01: Split dataframe:


# R - Decision Tree
install.packages("rpart")
install.packages("party")
library(rpart)
library(party)
library(dplyr)
library(caTools) 
#1)
city_temp<- read.csv("city_temperature.csv", stringsAsFactors=T)
cityt <- city_temp[c(250500:251500),]    #we take only first 500 rows as the initial dataset is too big, more than 2 mln records
#cityt[sapply(cityt, is.factor)] <- data.matrix(cityt[sapply(cityt, is.factor)])
str(cityt)
is.data.frame(cityt)

sample.split(cityt,SplitRatio = 0.70) -> split_tag
city_train <- subset(cityt, split_tag==T)
city_test <- subset(cityt, split_tag==F)

nrow(city_train)   #626
nrow(city_test)    #375

#2)
customer_churn<-read.csv("Customer_churn.csv",stringsAsFactors = T)
str(customer_churn)

sample.split(customer_churn, SplitRatio = 0.80)-> split_tag 
cc_train <- subset(customer_churn, split_tag==T)
cc_test <- subset(customer_churn, split_tag==F)

nrow(cc_train)   #5366
nrow(cc_test)    #1677


#3)
pharma <- read.csv('pharma_audit_data.csv', stringsAsFactors=T)
str(pharma)

sample.split(pharma, SplitRatio = 0.75)-> split_tag 
pharma_train <- subset(pharma, split_tag==T)
pharma_test <- subset(pharma, split_tag==F)

nrow(pharma_train)   #575
nrow(pharma_test)    #201

medical<- read.csv("medical.csv", stringsAsFactors=T)
medic <- medical[c(1:3000),] 
str(medic)


sample.split(medic, SplitRatio = 0.75)-> split_tag 
medic_train <- subset(medic, split_tag==T)
medic_test <- subset(medic, split_tag==F)

nrow(medic_train)   #2143
nrow(medic_test)    #857

library(caTools)


#Module 07, Assignment 02:  #Misclassification Rate

#1)
city_dt_model <- rpart(Region ~ AvgTemperature,data = city_train) # . in the formula of ML model, 
#it means we want to use all variables on the dataset as independent variables
plot(city_dt_model)
text(city_dt_model, cex = 0.9)
#Summary of the tree
install.packages("rattle")
library(rattle)
fancyRpartPlot(city_dt_model,palettes=c("Blues", "Greens"),cex = 0.8)
summary(city_dt_model)

# Misclassification Rate = (false positive + false negative) / (total predictions)
'''
Call:
rpart(formula = Region ~ AvgTemperature, data = city_train)
  n= 626 

         CP nsplit rel error    xerror       xstd
1 0.3723849      0 1.0000000 1.0000000 0.05085918
2 0.0100000      1 0.6276151 0.6317992 0.04478682
Node number 1: 626 observations,    complexity param=0.3723849
  predicted class=Africa  expected loss=0.3817891  P(node) =1
    class counts:   387   239
   probabilities: 0.618 0.382 
  left son=2 (523 obs) right son=3 (103 obs)
  Primary splits:
      AvgTemperature < 81.65 to the left,  improve=74.6551, (0 missing)

Node number 2: 523 observations
  predicted class=Africa  expected loss=0.2734226  P(node) =0.8354633
    class counts:   380   143
   probabilities: 0.727 0.273 
'''
#Misclassification Rate:
(387+239)/626      # 100%
(380+143)/523       # 100%

#2)

cc_dt_model <- rpart(Churn ~ MonthlyCharges, data = cc_train) # . in the formula of ML model, 
#it means we want to use all variables on the dataset as independent variables
plot(cc_dt_model)
text(ccc_dt_model, cex = 0.7)
#Summary of the tree
install.packages("rattle")
library(rattle)
fancyRpartPlot(cc_dt_model,palettes=c("Blues", "Greens"),cex = 0.8)
summary(cc_dt_model)


#Misclassification Rate = (false positive + false negative) / (total predictions)

'''
Call:
rpart(formula = Churn ~ MonthlyCharges, data = cc_train)
  n= 5366 

  CP nsplit rel error xerror xstd
1  0      0         1      0    0

Node number 1: 5366 observations
  predicted class=No  expected loss=0.2691018  P(node) =1
    class counts:  3922  1444
   probabilities: 0.731 0.269 
'''
#Misclassification Rate:

(3922+1444)/nrow(cc_train)    # 100%

#3)
medic_dt_model <- rpart(No.show ~ Age, data = medic_train) # . in the formula of ML model, 
#it means we want to use all variables on the dataset as independent variables
plot(medic_dt_model)
text(medic_dt_model, cex = 0.7)
#Summary of the tree
install.packages("rattle")
library(rattle)
fancyRpartPlot(medic_dt_model,palettes=c("Blues", "Greens"),cex = 0.8)
summary(medic_dt_model)

# Misclassification Rate = (false positive + false negative) / (total predictions)

'''

Call:
rpart(formula = No.show ~ Age, data = medic_train)
  n= 2143 

  CP nsplit rel error xerror xstd
1  0      0         1      0    0

Node number 1: 2143 observations
  predicted class=No  expected loss=0.2006533  P(node) =1
    class counts:  1713   430
   probabilities: 0.799 0.201 

'''
# Misclassification Rate:
(1713+430)/nrow(medic_train)   # 100%



#Module 07, Assignment 03: Indep and dep features:


diabet_model0 <- rpart(Outcome ~ . ,data = diabet_train) # . in the formula of ML model, 
#it means we want to use all variables on the dataset as independent variables
plot(diabet_model0)
text(diabet_model0, cex = 0.7)


city_dt_model0 <- rpart(Region ~ . , data = city_train) # . in the formula of ML model, 
#it means we want to use all variables on the dataset as independent variables
plot(city_dt_model0)
text(city_dt_model0, cex = 0.9)

cc_dt_model0 <- rpart(Churn ~ . , data = cc_train) # . in the formula of ML model, 
#it means we want to use all variables on the dataset as independent variables
plot(cc_dt_model0)
text(ccc_dt_model0, cex = 0.7)

medic_dt_model0 <- rpart(No.show ~ . , data = medic_train) # . in the formula of ML model, 
#it means we want to use all variables on the dataset as independent variables
plot(medic_dt_model0)
text(medic_dt_model0, cex = 0.7)


#Module 07, Assignment 04: Dtree model:

city_dt_model <- rpart(Region ~ AvgTemperature,data = city_train)
cc_dt_model <- rpart(Churn ~ MonthlyCharges, data = cc_train)
pharma_dt_model <- rpart(DrugId ~ Age, data = medic_train)
medic_dt_model <- rpart(No.show ~ Age, data = medic_train)


#Module 07, Assignment 05: Confusion matrix for city, churn, pharma:
#Module 07, Assignment 06: Accuracy for city, churn, pharma:
#1)
#Prediction of test dataset:
pred_city <- predict(city_dt_model0,newdata = city_test,type = "class") # classification
head(pred_city)
#Lets compare with actual
table(city_test$Region,pred_city)
(232+143)/nrow(city_test)  # Accuracy - 100%
(232)/(232+20) # Recall - 92%
(232)/(232+12) # Precision - 95%


#2)
pred_churn <- predict(cc_dt_model0, newdata = cc_test,type = "class") # classification
head(pred_churn)
#Lets compare with actual
table(cc_test$Churn, pred_churn)
(1145+140)/nrow(cc_test)  # Accuracy - 76%
(140)/(140+285) # Recall - 76%
(140)/(140+107) # Precision - 32%

'''
    pred_churn
        No  Yes
  No  1145  107
  Yes  285  140
  
  
# TP=140
# FN=285
# TN=1145
# FP=107
#Actual Predicted
#   TN      FP
#   FN      TP

'''
#3)
pred_med <- predict(medic_dt_model0,newdata = medic_test,type = "class") # classification
head(pred_med)
#Lets compare with actual
table(medic_test$No.show, pred_med)
(695+10)/nrow(medic_test)  # Accuracy - 82%
(10)/(10+148) # Recall - 82%
(10)/(10+4) # Precision - 6%

'''
  pred_med
       No Yes
  No  695   4
  Yes 148  10
  '''


#Module 07, Assignment 07: Accuracy for city, churn, pharma - see previous Assignment 06 (above)









