#Project 07 - Adult Census Income Classification project:

# Income Classification study on Adult Census Data:

#https://www.kaggle.com/code/limmyoungjin/r-t2-3-adult-census-income
install.packages("data.table")
install.packages("ggplot2")
install.packages("stats")
install.packages("stringr")
install.packages("extrafont")
install.packages("knitr")
install.packages("lmtest")
install.packages("sandwich")
install.packages("zoo")
library("data.table")
library("ggplot2")
library("stats")
library("stringr")
library("extrafont")
library("knitr")
library("lmtest")
library("sandwich")
library("zoo")
library(readr)
options(Scipen = 20)
options(scipen = 20)

# Other Functions & Libraries

install.packages("randomForest")
install.packages("xgboost")
install.packages("class")

library("randomForest")  # For Random Forest Exploration.
library("xgboost")       # In order to use Gradient Boosting algorithm.
library("class")         # In order to use k nearest neighbours classification algorithm.
f1score = function(precision, recall){2 * precision * recall / (precision + recall)}

data=read.csv('/Users/lana-n/datacsv/adult0.csv')
data[data=="?"]=NA

set.seed(1)
idx=sample(1:nrow(data),nrow(data)*0.8)
x_train=data[idx,-15]; y_train=data[idx,15]
x_test=data[-idx,-15]; y_test=data[-idx,15]

dim(x_train); dim(y_train); dim(x_test); dim(y_test)


apply(is.na(x_train),2,sum) # workclass(1467), occupation(1470), native.country(481)
apply(is.na(y_train),2,sum)
apply(is.na(x_test),2,sum) # workclass(369), occupation(373), native.country(102)

my_na=function(data){
  mode=names(which.max(table(data)))
  data[is.na(data)]=mode
  return(data)
}
x_train$workclass=my_na(x_train$workclass)
x_train$occupation=my_na(x_train$occupation)
x_train$native.country=my_na(x_train$native.country)
x_test$workclass=my_na(x_test$workclass)
x_test$occupation=my_na(x_test$occupation)
x_test$native.country=my_na(x_test$native.country)

str(x_train); str(y_train)

x_train$workclass=as.factor(x_train$workclass)
x_train$education=as.factor(x_train$education)
x_train$marital.status=as.factor(x_train$marital.status)
x_train$occupation=as.factor(x_train$occupation)
x_train$relationship=as.factor(x_train$relationship)
x_train$race=as.factor(x_train$race)
x_train$sex=as.factor(x_train$sex)
x_train$native.country=as.factor(x_train$native.country)
y_train$income=as.factor(y_train$income)

x_test$workclass=as.factor(x_test$workclass)
x_test$education=as.factor(x_test$education)
x_test$marital.status=as.factor(x_test$marital.status)
x_test$occupation=as.factor(x_test$occupation)
x_test$relationship=as.factor(x_test$relationship)
x_test$race=as.factor(x_test$race)
x_test$sex=as.factor(x_test$sex)
x_test$native.country=as.factor(x_test$native.country)

str(x_train)
str(x_test)
#  native.country 
y_train=y_train[x_train$native.country!="Holand-Netherlands",]
x_train=x_train[x_train$native.country!="Holand-Netherlands",]
x_train$native.country=as.factor(as.character(x_train$native.country))

str(x_train)
str(y_train)

set.seed(1117)
idx=sample(1:nrow(x_train),nrow(x_train)*0.7)
X_valid=x_train[-idx,]; Y_valid=y_train[-idx,]
X_train=x_train[idx,]; Y_train=y_train[idx,]


library(caret)
ran=preProcess(X_train,'range')
x_train_norm=predict(ran,X_train)
x_valid_norm=predict(ran,X_valid)
x_test_norm=predict(ran,x_test)

# Random Forest
library(randomForest)
set.seed(2021)
model_rf=randomForest(Y_train$income~.,data=x_train_norm,ntree=300)
y_rf=predict(model_rf,x_valid_norm)

# Accuracy
library(caret)
confusionMatrix(y_rf,Y_valid$income)[['overall']]['Accuracy'] # 0.8628

# test set
test_rf=predict(model_rf,x_test_norm)

# 
confusionMatrix(test_rf,as.factor(y_test$income))[['overall']]['Accuracy'] # 0.8621



















