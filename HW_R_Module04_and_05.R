
#R for Data Science - class August-Sept 2022 with Prateek
#Homework
#Module 04, Assignment 01:  Calcucate IQR for some columns for placement dataset, cc, pharma.


install.packages("caTools")

library(dplyr)
library(caTools)


#CREATE DATAFRAME FROM FILE:
customer_churn<- read.csv("Customer_Churn.csv")
str(customer_churn)
is.data.frame(customer_churn)


placement<- read.csv("Placement_Data_Full_Class.csv")
str(placement)
is.data.frame(placement)

pharma <- read.csv('pharma_audit_data.csv')
str(pharma)


#Task 1: iqr for placement:

#The interquartile range of an observation variable is the difference of its upper and lower quartiles. 
#It is a measure of how far apart the middle portion of data spreads in value.

IQR(placement$hsc_p)

IQR(placement$ssc_p)

IQR(placement$degree_p)

IQR(placement$mba_p)

IQR(placement$etest_p)


#Task 2: iqr for cc:

IQR(customer_churn$TotalCharges, na.rm = T)   ## na.rm = TRUE ignores missing values

IQR(customer_churn$MonthlyCharges, na.rm = T)

IQR(customer_churn$tenure, na.rm = T)

#Task 3: iqr for pharma:

IQR(pharma$Age, na.rm = T)  #there is no Age column in my pharma dayaset, soI will use medical dataset:

#so I just downloaded similar dataset from kaggle using the following link:

# https://www.kaggle.com/datasets/joniarroba/noshowappointments?resource=download

#it does not have Issues column but instead has diabetes column which I can use
#it does not have Drug_id  column but instead has AppointmentID column which I can use
# it has PatientID,Gender and Age columns  
medical<- read.csv("medical.csv")
str(medical)

IQR(medical$Age, na.rm = T) 

IQR(medical$PatientId, na.rm = T) 

#---------------------------------------------------
#Module 04, Assignment 02:  Calcucate Variability and Spread for some columns for placement dataset, cc, pharma.

#Task 1:

mean(customer_churn$tenure)       #32
median(customer_churn$tenure)     #29
range(customer_churn$tenure)       # 0 - 72
IQR(customer_churn$tenure)          #46
var(customer_churn$tenure)         #603
sd(customer_churn$tenure)         #std = 24

#Task 2:

mean(placement$ssc_p)       #67
median(placement$ssc_p)     #67
range(placement$ssc_p)       # 40 - 89
IQR(placement$ssc_p)          #15
var(placement$ssc_p)         #117
sd(placement$ssc_p)      #std = 10.8


#Task 3:

mean(medical$Age)       #37
median(medical$Age)     #37
range(medical$Age)       # 1 - 115
IQR(medical$Age)          #15
var(medical$Age)         #5341
sd(medical$Age)      #std = 23.1



#_____________________________________


#R for Data Science - class August-Sept 2022 with Prateek
#Homework
#Module 05  ML and LinRegression, Assignment 01-12:  Indep and dependent features:
#Assignment 01:

city_temp<- read.csv("city_temperature.csv")
cityt <- city_temp[c(1:500),]    #we take only first 500 rows as the initial dataset is too big
str(city_temp)
str(cityt)
is.data.frame(cityt)


#8 total variables: 7 of then independent: Region Country State  City Month Day  Year
#one dependent: AvgTemperature - need to be predicted


placement<- read.csv("Placement_Data_Full_Class.csv")
str(placement)
is.data.frame(placement)

#all var independant, except 'status' (Placed, Not Placed - need to be predicted)

pharma <- read.csv('pharma_audit_data.csv')
str(pharma)
#all var independant, except 'Risk' (0 or 1 - need to be predicted) 

medical<- read.csv("medical.csv")
str(medical)

#all var independant, except 'No_show' (Yes or No - need to be predicted, is the patient will show up for his/her appointment) 

#Module 05   Assignment 02: Split datasets

library(caTools)
#1)
sample.split(cityt,SplitRatio = 0.70)-> split_tag
city_temp_train <- subset(cityt, split_tag==T)
city_temp_test <- subset(cityt, split_tag==F)

#2)
sample.split(placement, SplitRatio = 0.80)-> split_tag
placement_train <- subset(placement, split_tag==T)->train
placement_test <- subset(placement, split_tag==F)->test
#3)

sample.split(pharma, SplitRatio = 0.75)-> split_tag
pharma_train <- subset(pharma, split_tag==T)->train
pharma_test <- subset(pharma, split_tag==F)->test

sample.split(medical, SplitRatio = 0.75)-> split_tag
medical_train <- subset(medical, split_tag==T)->train
medical_test <- subset(medical, split_tag==F)->test


#Module 05   Assignment 03: Build multiple lin regression model:

city_temp_lr_model <- lm(Month ~ Region + Country + City + AvgTemperature, data = city_temp_train)

city_temp_lr_model1 <- lm(Month ~ AvgTemperature, data = city_temp_train)

placement_lr_model <- lm(etest_p ~ degree_p + mba_p + status, data = placement_train)

#pharma_lr_model <- lm(Age ~ Issues + DrugId + Gender, data = pharma_train)   #Error in eval(predvars, data, env) : object 'Age' not found
pharma_lr_model <- lm(Audit_Risk ~ PARA_A + Risk_C + Inherent_Risk, data = pharma_train)

medical_lr_model <- lm(Age ~ Scholarship + Hipertension + Diabetes + Gender, data = medical_train)


summary(city_temp_lr_model)

#Residual standard error: 3.444 on 1816133 degrees of freedom
#Multiple R-squared:  0.007413,	Adjusted R-squared:  0.007237 
#F-statistic: 42.25 on 321 and 1816133 DF,  p-value: < 2.2e-16

summary(city_temp_lr_model1)

#Residual standard error: 3.234 on 311 degrees of freedom
#Multiple R-squared:  0.1296,	Adjusted R-squared:  0.1268 
#F-statistic: 46.32 on 1 and 311 DF,  p-value: 5.165e-11


summary(placement_lr_model)

#Residual standard error: 12.76 on 168 degrees of freedom
#Multiple R-squared:  0.09053,	Adjusted R-squared:  0.07429 
#F-statistic: 5.574 on 3 and 168 DF,  p-value: 0.00114


summary(pharma_lr_model)

#Residual standard error: 9.06 on 570 degrees of freedom
#Multiple R-squared:  0.6965,	Adjusted R-squared:  0.6949 
#F-statistic: 436.1 on 3 and 570 DF,  p-value: < 2.2e-16

summary(medical_lr_model)

#Residual standard error: 19.69 on 78943 degrees of freedom
#Multiple R-squared:  0.2761,	Adjusted R-squared:  0.276 
#F-statistic:  7526 on 4 and 78943 DF,  p-value: < 2.2e-16

#Module 5 Assignment 6: Multilinear model:  and #Module 5 Assignment 9: RMSE for Multilinear model:

#1)
#Predict test data
predicted_values_cityt <- predict(city_temp_lr_model, city_temp_test)  # Test  
View(predicted_values_cityt)

#Bind Predictions with actual values
final_data_cityt <- cbind(Actual=city_temp_test$Month, Predicted=predicted_values_cityt)

head(final_data_cityt)
as.data.frame(final_data_cityt)->final_data_cityt
final_data_cityt$error <-final_data_cityt$Actual - final_data_cityt$Predicted
head(final_data_cityt)                    #Result1 

#Root Mean Squared Error
#Calculating root mean squared-error of the predictions
rmse1<-sqrt(mean((final_data_cityt$error)^2))
rmse1

#Since RMSE is not great, we can conclude that this is not a great model
mape <- rmse1/mean(city_temp_test$Month)
rmse1/mean(city_temp_test$Month) # < 10% 


#2)

#Predict test data
predicted_values_place <- predict(placement_lr_model, placement_test)  # Test  
View(predicted_values_place)

#Bind Predictions with actual values
final_data_place <- cbind(Actual=placement_test$etest_p, Predicted=predicted_values_place)

head(final_data_place)
as.data.frame(final_data_place)->final_data_place
final_data_place$error <-final_data_place$Actual - final_data_place$Predicted
head(final_data_place)                    #Result2 

#Root Mean Squared Error
#Calculating root mean squared-error of the predictions
rmse2<-sqrt(mean((final_data_place$error)^2))
rmse2

#Since RMSE is not great, we can conclude that this is not a great model
mape2 <- rmse2/mean(placement_test$etest_p)
rmse2/mean(placement_test$etest_p) #should be  < 10% but we have here 18%

#3)

#Predict test data
predicted_values_pharma <- predict(pharma_lr_model, pharma_test)  # Test  
View(predicted_values_pharma)

#Bind Predictions with actual values
final_data_pharma <- cbind(Actual=pharma_test$Audit_Risk, Predicted=predicted_values_pharma)

head(final_data_pharma)
as.data.frame(final_data_pharma)->final_data_pharma
final_data_pharma$error <-final_data_pharma$Actual - final_data_pharma$Predicted
head(final_data_pharma)                    #Result3 

#Root Mean Squared Error
#Calculating root mean squared-error of the predictions
rmse3<-sqrt(mean((final_data_cityt$error)^2))
rmse3

#Since RMSE is not great, we can conclude that this is not a great model
mape3 <- rmse3/mean(pharma_test$Audit_Risk)
rmse3/mean(city_temp_test$Month) # < 10% 


#Module 5 Assignment 7: Plot regression line:
#1)
#Plot data to see if there is relationship

plot(Month~AvgTemperature, data=city_temp_train,col="red")

#Fit regresion model between Monthly charges and Tenure

abline(lm(Month~AvgTemperature,data=city_temp_train)) # overlaying linear regression linear scatter plot
#2) placement

plot(etest_p~status, data=placement_train,col="green")

abline(lm(etest_p ~ status,data=placement_train))

#3) pharma
plot(Audut_Risk~Risk_C, data=pharma_train,col="blue")

abline(lm(Audit_Risk~Risk_C,data=pharma_train))


#Module 5 Assignment 8: Simple linear model:

city_model <- lm(Month ~ AvgTemperature, data = city_temp_train)

placement_model <- lm(etest_p ~ mba_p, data = placement_train)

#pharma_lr_model <- lm(Age ~ Issues + DrugId + Gender, data = pharma_train)   #Error in eval(predvars, data, env) : object 'Age' not found
pharma_model <- lm(Audit_Risk ~Risk_C, data = pharma_train)

medical_model <- lm(Age ~ Diabetes, data = medical_train)


summary(city_model)

#Residual standard error: 3.234 on 311 degrees of freedom
#Multiple R-squared:  0.1296,	Adjusted R-squared:  0.1268 
#F-statistic: 46.32 on 1 and 311 DF,  p-value: 5.165e-11

summary(placement_model)

#Residual standard error: 12.9 on 170 degrees of freedom
#Multiple R-squared:  0.05832,	Adjusted R-squared:  0.05278 
#F-statistic: 10.53 on 1 and 170 DF,  p-value: 0.001416


summary(pharma_model)

#Residual standard error: 17.16 on 573 degrees of freedom
#Multiple R-squared:  0.1474,	Adjusted R-squared:  0.146 
#F-statistic:  99.1 on 1 and 573 DF,  p-value: < 2.2e-16


summary(medical_model)
#Residual standard error: 22.11 on 78946 degrees of freedom
#Multiple R-squared:  0.08638,	Adjusted R-squared:  0.08637 
#F-statistic:  7464 on 1 and 78946 DF,  p-value: < 2.2e-16

#Assignment 9 and 10 - see in Assignment 6 (RMSE calculated for 3 models)


#Module 5 #Assignment 11 - Assumption - analyse feature of the odel:

#1)
'''
summary(city_temp_lr_model1)

Call:
  lm(formula = Month ~ AvgTemperature, data = city_temp_train)

Residuals:
  Min      1Q  Median      3Q     Max 
-4.7279 -2.3896 -0.9342  1.5874  8.9702 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)     0.73267    0.72414   1.012    0.312    
AvgTemperature  0.07781    0.01143   6.806 5.16e-11 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
'''

#Conclusion: AvgTemperature is significant feature for the model (based on Signif.codes)

#2)
'''
summary(placement_lr_model)

Call:
  lm(formula = etest_p ~ degree_p + mba_p + status, data = placement_train)

Residuals:
  Min       1Q   Median       3Q      Max 
-29.2815 -10.7224  -0.7807  10.1581  30.1328 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)  
(Intercept)   24.4399    12.0964   2.020   0.0449 *
  degree_p       0.3620     0.1659   2.183   0.0304 *
  mba_p          0.3878     0.1876   2.068   0.0402 *
  statusPlaced  -0.2277     2.3652  -0.096   0.9234  
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
'''
#Conclusion: degree_p and mba_p are significant features for the model (based on Signif.codes)


#3) 
'''
summary(pharma_lr_model)

Call:
  lm(formula = Audit_Risk ~ PARA_A + Risk_C + Inherent_Risk, data = pharma_train)

Residuals:
  Min      1Q  Median      3Q     Max 
-35.645  -0.906  -0.504  -0.343 125.420 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)   -6.679096   0.923787  -7.230 1.56e-12 ***
  PARA_A         0.283202   0.072702   3.895  0.00011 ***
  Risk_C         7.001363   0.751451   9.317  < 2e-16 ***
  Inherent_Risk  0.214812   0.009164  23.440  < 2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

'''

#Conclusion: Inherent_Risk and Risk_C and PARA_A are significant features for the model (based on Signif.codes)


#Module 05   Assignment 12: RMSE multiple lin regression model:

city_temp_lr_model <- lm(Month ~ Region + Country + City + AvgTemperature, data = city_temp_train)

placement_lr_model <- lm(etest_p ~ degree_p + mba_p + status, data = placement_train)

#pharma_lr_model <- lm(Age ~ Issues + DrugId + Gender, data = pharma_train)   #Error in eval(predvars, data, env) : object 'Age' not found
pharma_lr_model <- lm(Audit_Risk ~ PARA_A + Risk_C + Inherent_Risk, data = pharma_train)

medical_lr_model <- lm(Age ~ Scholarship + Hipertension + Diabetes + Gender, data = medical_train)

summary(city_temp_lr_model)
summary(placement_lr_model)
summary(pharma_lr_model)
summary(medical_lr_model)

#1)city-temp:

#Predict test data
predicted_values_cityt <- predict(city_temp_lr_model, city_temp_test)  # Test  
View(predicted_values_cityt)

#Bind Predictions with actual values
final_data_cityt <- cbind(Actual=city_temp_test$Month, Predicted=predicted_values_cityt)

head(final_data_cityt)
as.data.frame(final_data_cityt)->final_data_cityt
final_data_cityt$error <-final_data_cityt$Actual - final_data_cityt$Predicted
head(final_data_cityt)                    #Result1 

#Root Mean Squared Error
#Calculating root mean squared-error of the predictions
rmse01<-sqrt(mean((final_data_cityt$error)^2))
rmse01

#2)placement
#Predict test data

predicted_values_place <- predict(placement_lr_model, placement_test)
View(predicted_values_place)

#Bind Predictions with actual values
final_data_place <- cbind(Actual=placement_test$etest_p, Predicted=predicted_values_place)

head(final_data_place)
as.data.frame(final_data_place)->final_data_place
final_data_place$error <-final_data_place$Actual - final_data_place$Predicted
head(final_data_place)    

#Root Mean Squared Error
#Calculating root mean squared-error of the predictions
rmse02<-sqrt(mean((final_data_place$error)^2))
rmse02

#3)pharma or medical
#Predict test data
predicted_values_pharma <- predict(pharma_lr_model, pharma_test) 
View(predicted_values_pharma)

#Bind Predictions with actual values
final_data_pharma <- cbind(Actual=pharma_test$Audit_Risk, Predicted=predicted_values_pharma)

head(final_data_pharma)
as.data.frame(final_data_pharma)->final_data_pharma
final_data_pharma$error <-final_data_pharma$Actual - final_data_pharma$Predicted
head(final_data_pharma)

#Root Mean Squared Error
#Calculating root mean squared-error of the predictions
rmse03<-sqrt(mean((final_data_cityt$error)^2))
rmse03

