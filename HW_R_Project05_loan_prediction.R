#Project 05 - Loan Prediction:


#https://www.kaggle.com/datasets/vikasukani/loan-eligible-dataset?select=loan-test.csv taken from here Loan Eligible dataset
loan<- read.csv("loan-train.csv")   #614 records
str(loan)
print(head(loan))
View(loan)
#loan<- read.csv("loan-train.csv", stringsAsFactors=T)
loan[!complete.cases(loan),]

#apply(loan,2, function(x) is.na(x))
which(is.na(loan))
loan1 <- na.omit(loan)
which(is.na(loan1))
#apply(loan1,2, function(x) is.na(x))

str(loan1)
print(head(loan1))

loan1$Gender <- as.factor(loan1$Gender)
loan1$Married <- as.factor(loan1$Married)
loan1$Education <- as.factor(loan1$Education)
loan1$Self_Employed <- as.factor(loan1$Self_Employed)
loan1$Property_Area <- as.factor(loan1$Property_Area)
loan1$Loan_Status <- as.factor(loan1$Loan_Status)

print(head(loan1))

write.csv(loan1, "loan1.csv")

loan2<- read.csv("loan1.csv", stringsAsFactors=T)   #614 records
str(loan2)
loan2 <- loan2[, c(3:14)]
str(loan2)
print(head(loan2))

#View(loan2)
#Split the data into training and testing -> 80% train and 20% test

library(caTools)

sample.split(loan2$Loan_Status,SplitRatio = 0.80)-> split_tag
loan2_train <- subset(loan2, split_tag==T)
loan2_test <- subset(loan2, split_tag==F)

#Building decision trees
#For rpart() we need to load rpart library

install.packages("rpart")
library(rpart)

#Lets use all variables to build our decision tree model

loan2_model <- rpart(Loan_Status ~ Property_Area + Credit_History + Gender + Married + LoanAmount ,data = loan2_train) # . in the formula of ML model, 
#it means we want to use all variables on the dataset as independent variables
plot(loan2_model)
text(loan2_model)
plot(loan2_model)
text(loan2_model, cex = 0.7)

#Summary of the tree
install.packages("rattle")
library(rattle)
fancyRpartPlot(loan2_model,palettes=c("Blues", "Greens"),cex = 0.8)

summary(loan2_model)

#Prediction of test dataset
pred_loan2 <- predict(loan2_model,newdata = loan2_test,type = "class") # classification
head(pred_loan2)

#Lets compare with actual
table(loan2_test$Loan_Status,pred_loan2)
(71+14)/(71+14+19+2)  # Accuracy - 80%
(71)/(71+2) # Recall - 97%
(71)/(71+19) # Precision - 78%
#pred_loan2
#N  Y
#N 14 19
#Y  2 71

# TP=69
# FN=4
# TN=13
# FP=20
#Actual Predicted
#   TN      FP
#   FN      TP
#pred_loan2
#N  Y
#N 13 20
#Y  4 69

(69+13)/(69+13+20+4) # Accuracy   #Accuracy=(TP+TN)/(TP+FP+FN+TN)   #77%
69/(69+4) # Recall       #Recall=TP/(TP+FN)                #98%
69/(69+20) # Precision   #Precision=TP/(TP+FP)             #45%



#Logistic Regression with glm for Loan dataset:

model_log_loan2 <- glm(Loan_Status ~ . ,data = loan2_train, family = "binomial") # binary classification 

model_log_loan3 <- glm(Loan_Status ~ Credit_History,data = loan2_train, family = "binomial") # binary classification
summary(model_log_loan3)

predict(model_log_loan2,newdata=loan2_test,type="response")->result_mod_log_loan2
head(result_mod_log_loan2)  #check
range(result_mod_log_loan2)
table(loan2_test$Loan_Status, result_mod_log_loan2)


predict(model_log_loan3,newdata=loan2_test,type="response")->result_mod_log_loan3
head(result_mod_log_loan3)  #check
range(result_mod_log_loan3)
table(loan2_test$Loan_Status, result_mod_log_loan3)

'''
 result_mod_log_loan3
    0.112903225904726 0.792243767312942
  N                17                16
  Y                 0                73
'''
(73+17)/(17+73+16+0) # Accuracy   #Accuracy=(TP+TN)/(TP+FP+FN+TN)   #85%
73/(73+0) # Recall       #Recall=TP/(TP+FN)                #98%
73/(73+16) # Precision   #Precision=TP/(TP+FP)             #45%

