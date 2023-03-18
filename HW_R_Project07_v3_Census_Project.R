#Project 07 - Adult Census Income Classification project:

# Income Classification study on Adult Census Data:

#https://www.kaggle.com/code/kazimanil/adult-census-income-classification/notebook
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
library("corrplot")
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


adult = read.csv('/Users/lana-n/datacsv/adult0.csv')

# Data Intake
adult = fread('/Users/lana-n/datacsv/adult0.csv')
# Initial Manipulation
str(adult)


#Setting unwanted values as NA
#Adult[Adult == "?"] <- NA

#Omitting NA values:
adult <- na.omit(adult)

#'Re-factoring' variables to exclude the unwanted levels:
adult$workclass <- as.factor(adult$workclass)
adult$education <- as.factor(adult$education)
adult$marital.status <- as.factor(adult$marital.status)
adult$occupation <- as.factor(adult$occupation)
adult$relationship <- as.factor(adult$relationship)
adult$race <- as.factor(adult$race)
adult$sex <- as.factor(adult$sex)
adult$native.country <- as.factor(adult$native.country)
str(adult)

#Changing income to 0, 1:
adult$income <- as.factor(adult$income)
library(plyr)
adult$income <- mapvalues(adult$income, from = c('>50K','<=50K'), to = c(1,0))

str(adult$income)

hist(adult$age)
hist(adult$fnlwgt)
hist(adult$education.num)

pairs(~income + fnlwgt, data = adult) # fnlwgt can be ignored
pairs(~income + education.num, data = adult) # education.num can be ignored

#adult <- adult[,-3] # remove fnlwgt
#adult <- adult[,-4] # remove education.num
str(adult)


sum(is.na(adult$workclass) == TRUE)/32561
sum(is.na(adult$occupation) == TRUE)/32561
sum(is.na(adult$native.country) == TRUE)/32561


adult <- adult[!is.na(adult$workclass),]
# 30725 rows

sum(is.na(adult$occupation) == TRUE) # 7 rows with NA for occupation to be deleted
adult <- adult[!is.na(adult$occupation),]
# 30718 rows

sum(is.na(adult$native.country) == TRUE) # 556 rows with NA for native.country

adult$native.country[is.na(adult$native.country)] <- names(which.max(table(adult$native.country)))

#split the dataset:
library("caTools")
set.seed(42)
split <- sample.split(adult, SplitRatio = 0.7) # 70:30
train <- subset(adult, split == TRUE)
test <- subset(adult, split == FALSE)

sample.split(adult,SplitRatio = 0.70) -> split_tag
train <- subset(adult, split_tag==T)
test <- subset(adult, split_tag==F)

str(train)
str(test)

#Removing fnlwgt, education.num, capital.loss, capital.gain, and native.country from Adult
adult <- adult[, -c(3, 4, 11, 12, 14)]
#Reordering to place 'income' in the first column
adult <- adult[,c(10, 1:9, 11)]
attach(adult)
#Split data into train and test set
set.seed(1)
train <- sample(1:length(income), length(income)*0.5)
adult.train <- adult[train,]
adult.test <- adult[-train,]
str(adult)

train <- adult.train
test <- adult.test
str(train)
str(test)

#train the model - decision tree:
library("rpart")
library("rpart.plot")
# build decision tree with train set and method as "class" for classification
decision_tree <- rpart(formula = income~., data = train, method = "class")
# plot the decision tree
rpart.plot(decision_tree, box.palette = 'RdBu')

test$predicted.income <- predict(decision_tree, test, type = "class")


confMat <- table(test$predicted.income, test$income)
accuracy <- sum(diag(confMat))/sum(confMat)
print("Confusion matrix")
confMat
print("Accuracy:")
accuracy   #82%



#Random forest:
library("randomForest")
random_forest <- randomForest(income~., data = train, method = "class", ntree = 500, do.trace = 100)
test$rf.predicted.income <- predict(random_forest, test, type = "class")
rfconfMat <- table(test$rf.predicted.income, test$income)
rfaccuracy <- sum(diag(rfconfMat))/sum(rfconfMat)
print("Confusion matrix for random forest")
rfconfMat
print("Accuracy of random forest:")
rfaccuracy  #83%

#Plotting ROC curves and getting AUC for decision tree and random forest:
library("pROC")
# i. decision tree
predictionprobs <- predict(decision_tree, test, type = "prob")
auc <- auc(test$income,predictionprobs[,2])
# Area under the curve: 0.8435
plot(roc(test$income,predictionprobs[,2]))

# ii. random forest
rfpredictionprobs <- predict(random_forest, test, type = "prob")
rfauc <- auc(test$income,rfpredictionprobs[,2])
# Area under the curve: 0.9018
plot(roc(test$income,rfpredictionprobs[,2]))


































