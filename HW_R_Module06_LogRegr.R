#R for Data Science - class August-Sept 2022 with Prateek
#Homework
#Module 06, Assignment 01 - 08:  Logistic regression for placement dataset, cc, pharma.

#Module 06, Assignment 01:

install.packages("caTools")

#library(dplyr)
library(caTools)

#Task 1: Split cityt dataset 70-30:

city_temp<- read.csv("city_temperature.csv", stringsAsFactors=T)
cityt <- city_temp[c(250500:251500),]    #we take only first 500 rows as the initial dataset is too big, more than 2 mln records
#cityt[sapply(cityt, is.factor)] <- data.matrix(cityt[sapply(cityt, is.factor)])
str(cityt)
is.data.frame(cityt)

sample.split(cityt,SplitRatio = 0.70) -> split_tag
cityt_train <- subset(cityt, split_tag==T)
cityt_test <- subset(cityt, split_tag==F)

nrow(cityt_train)   #625
nrow(cityt_test)    #375

# Task 2: Split cc dataset 80-20:

customer_churn<-read.csv("Customer_churn.csv",stringsAsFactors = T)
str(customer_churn)

sample.split(customer_churn, SplitRatio = 0.80)-> split_tag 
subset(cc_train, split_tag==T)->cc_train
subset(cc_test, split_tag==F)->cc_test

nrow(cc_train)   #4090
nrow(cc_test)    #398


placement<- read.csv("Placement_Data_Full_Class.csv", stringsAsFactors=T)
str(placement)
is.data.frame(placement)


# Task 3: Splot pharma dataset 75-25:

pharma <- read.csv('pharma_audit_data.csv', stringsAsFactors=T)
str(pharma)

sample.split(pharma, SplitRatio = 0.75)-> split_tag 
pharma_train <- subset(pharma, split_tag==T)
pharma_test <- subset(pharma, split_tag==F)

nrow(pharma_train)   #575
nrow(pharma_test)    #201

medical<- read.csv("medical.csv", stringsAsFactors=T)
str(medical)


sample.split(medical, SplitRatio = 0.75)-> split_tag 
medical_train <- subset(medical, split_tag==T)
medical_test <- subset(medical, split_tag==F)

nrow(medical_train)   #778947
nrow(medical_test)    #31580

#Module 06, Assignment 02: Simpole Logistic Regression:
#1)
model_log_cityt <- glm(Region ~ AvgTemperature, data=cityt_train, family = "binomial") # binary classification 
summary(model_log_cityt)

#2)
model_log_cc <- glm(Churn ~ MonthlyCharges, data=cc_train, family = "binomial") # binary classification 
summary(model_log_cc)

#3)

model_log_medical <- glm(No.show ~ Age, data=medical_train, family = "binomial") # binary classification 
summary(model_log_medical)

#Module 06, Assignment 03: Significance of features:
#1)
model2_log_cityt <- glm(Region ~ AvgTemperature + Day + Month + Year, data=cityt_train, family = "binomial") # binary classification 
summary(model2_log_cityt)

'''
Coefficients:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)     6.149e+03  3.360e+06   0.002    0.999
AvgTemperature -4.729e-03  3.033e+02   0.000    1.000 ***
Day            -1.236e-02  1.599e+03   0.000    1.000
Month          -1.748e-01  4.303e+03   0.000    1.000
Year           -3.068e+00  1.676e+03  -0.002    0.999
'''

#Conclusion - the most significant feature for the model is AvgTemperature.

#2)
model2_log_cc <- glm(Churn ~ MonthlyCharges + tenure + StreamingTV + TotalCharges, data=cc_train, family = "binomial") # binary classification 
summary(model2_log_cc)
'''
Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)                    -0.9909692  0.2852132  -3.474 0.000512 ***
  MonthlyCharges                  0.0237186  0.0041610   5.700  1.2e-08 ***
  tenure                         -0.0872238  0.0113808  -7.664  1.8e-14 ***
  StreamingTVNo internet service -0.6742488  0.2477675  -2.721 0.006503 ** 
  StreamingTVYes                  0.1350235  0.1427766   0.946 0.344303    
TotalCharges                    0.0002808  0.0001270   2.210 0.027072 *  
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

'''
#Conclusion - the most significant feature for the model is MonthlyCHarges and Tenure, then StreamingTV.

#3)

model2_log_medical <- glm(No.show ~ Age + Gender + Diabetes + Alcoholism + Handcap, data=medical_train, family = "binomial") # binary classification 
summary(model2_log_medical)
'''
Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept) -1.120127   0.018286 -61.257   <2e-16 ***
  Age         -0.006884   0.000413 -16.668   <2e-16 ***
  GenderM     -0.039551   0.018893  -2.093   0.0363 *  
  Diabetes     0.056667   0.037285   1.520   0.1286    
Alcoholism   0.091409   0.052315   1.747   0.0806 .  
Handcap     -0.024687   0.057717  -0.428   0.6689    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

'''
#Conclusion - the most significant feature for the model is Age, then GenderM.



#Module 06, Assignment 4: Null and Residual Deviance:

#p-value = 1 - pchisq(deviance, degrees of freedom)

#1) 
'''
Call:
glm(formula = Region ~ AvgTemperature + Day + Month + Year, family = "binomial", 
    data = cityt_train)

Deviance Residuals: 
  Min          1Q      Median          3Q         Max  
-6.051e-06  -1.564e-06  -8.873e-07   1.783e-06   6.173e-06  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)     6.149e+03  3.360e+06   0.002    0.999
AvgTemperature -4.729e-03  3.033e+02   0.000    1.000
Day            -1.236e-02  1.599e+03   0.000    1.000
Month          -1.748e-01  4.303e+03   0.000    1.000
Year           -3.068e+00  1.676e+03  -0.002    0.999

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 8.3153e+02  on 624  degrees of freedom
Residual deviance: 3.8927e-09  on 620  degrees of freedom
AIC: 10

Number of Fisher Scoring iterations: 25
'''
#Conclusion: the model is good as the Null Deviace and Residual deviance are reallu small as well as AIC.

#2)
'''
Call:
  glm(formula = Churn ~ MonthlyCharges + tenure + StreamingTV + 
        TotalCharges, family = "binomial", data = cc_train)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-1.8590  -0.6803  -0.3428   0.7490   3.3419  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)                    -0.9909692  0.2852132  -3.474 0.000512 ***
  MonthlyCharges                  0.0237186  0.0041610   5.700  1.2e-08 ***
  tenure                         -0.0872238  0.0113808  -7.664  1.8e-14 ***
  StreamingTVNo internet service -0.6742488  0.2477675  -2.721 0.006503 ** 
  StreamingTVYes                  0.1350235  0.1427766   0.946 0.344303    
TotalCharges                    0.0002808  0.0001270   2.210 0.027072 *  
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 2730.4  on 2369  degrees of freedom
Residual deviance: 2031.1  on 2364  degrees of freedom
(5 observations deleted due to missingness)
AIC: 2043.1
'''
# Conclusion: The model is not good and needs improvement.

#3)

'''
Call:
glm(formula = No.show ~ Age + Gender + Diabetes + Alcoholism + 
    Handcap, family = "binomial", data = medical_train)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.7634  -0.6972  -0.6490  -0.5983   2.0353  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept) -1.120127   0.018286 -61.257   <2e-16 ***
Age         -0.006884   0.000413 -16.668   <2e-16 ***
GenderM     -0.039551   0.018893  -2.093   0.0363 *  
Diabetes     0.056667   0.037285   1.520   0.1286    
Alcoholism   0.091409   0.052315   1.747   0.0806 .  
Handcap     -0.024687   0.057717  -0.428   0.6689    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 79371  on 78946  degrees of freedom
Residual deviance: 79075  on 78941  degrees of freedom
AIC: 79087

Number of Fisher Scoring iterations: 4
'''
#Conclusion: the model in not good, needs much improvement.


#Module 06, Assignment 5: Multiple logistic regression:

#1)
model2_log_cityt <- glm(Region ~ AvgTemperature + Day + Month + Year, data=cityt_train, family = "binomial") # binary classification 
summary(model2_log_cityt)



#2)
model2_log_cc <- glm(Churn ~ MonthlyCharges + tenure + StreamingTV + TotalCharges, data=cc_train, family = "binomial") # binary classification 
summary(model2_log_cc)

#3)

model2_log_medical <- glm(No.show ~ Age + Gender + Diabetes + Alcoholism + Handcap, data=medical_train, family = "binomial") # binary classification 
summary(model2_log_medical)


model2_log_pharma <- glm(DrugId ~ Age + Gender + Patient_Id, data=medical_train, family = "binomial") # binary classification 
summary(model2_log_medical)


#Module 06, Assignment 6: Indep and dependent features:


colnames(cityt)
#"Region"         "Country"        "State"          "City"           "Month"          "Day"            "Year"           "AvgTemperature"

model_all_features_log_cityt <- glm(Region ~ Country + State + City + Month + Day + Year + AvgTemperature, data=cityt_train, family = "binomial") # binary classification 
summary(model_all_features_log_cityt)
#check what features are marked **, those are best to use as indep features
colnames(customer_churn)
#same steps
colnames(medical)
#same steps
colnames(pharma)
#same steps


#Module 06, Assignment 7: Confusion Matrix and Accuracy:

#1)

sample.split(cityt$AvgTemperature,SplitRatio = 0.75)-> split_tag
subset(cityt, split_tag==T)->citytrain
subset(cityt, split_tag==F)->citytest

mod_city<-glm(Region ~ AvgTemperature + Day + Month + Year, data=citytrain, family = "binomial")
summary(mod_city)
predict(mod_city,newdata=citytest,type="response")->result_logcity
head(result_logcity)  #check
range(result_logcity)
table(citytest$Region, result_logcity > 0.15) 

'''
Africa                              133    0
  Asia                                  0   74
'''
'''
   FALSE TRUE
  No    74  6
  Yes    9  133
'''
# TP=133
# FN=6
# TN=74
# FP=9
#Actual Predicted
#   TN      FP
#   FN      TP

#Accuracy=(TP+TN)/nrow(cctest)      #93%
#Accuracy=(TP+TN)/(TP+FP+FN+TN):
(133+74)/(133+74+9+6) #=0.59

#Recall=TP/(TP+FN):               #95%
133/(133+6)
# Precision=TP/(TP+FP):           #93%
133/(133+9)
#F-measure=2*Precision*Recall/(Precision+Recall):   #93.9%
2*93*95/(93+95)

#2)

sample.split(customer_churn$Churn,SplitRatio = 0.75)-> split_tag
subset(customer_churn, split_tag==T)->cctrain
subset(customer_churn, split_tag==F)->cctest

mod_log2<-glm(Churn ~  tenure  + Contract + TechSupport + MonthlyCharges + OnlineSecurity + PhoneService + OnlineBackup + PaperlessBilling, data=cctrain, family = "binomial")
summary(mod_log2)
predict(mod_log2,newdata=cctest,type="response")->result_log2
head(result_log2)  #check
range(result_log2)
table(cctest$Churn, result_log2 > 0.15) 
nrow(cctest)  #1761

'''
   FALSE TRUE
  No    689  605
  Yes    29  438
'''
# TP=438
# FN=689
# TN=605
# FP=29

#Actual Predicted
#   TN      FP
#   FN      TP

#Accuracy=(TP+TN)/nrow(cctest)      #59%
#Accuracy=(TP+TN)/(TP+FP+FN+TN):
(438+605)/(1761) #=0.59

#Recall=TP/(TP+FN):               #38.8$
438/(438+689)
# Precision=TP/(TP+FP):           #93%
438/(438+29)
#F-measure=2*Precision*Recall/(Precision+Recall):   #54.9%
2*93*39/(93+39)

#3)

sample.split(medical$No.show,SplitRatio = 0.75)-> split_tag
subset(medical, split_tag==T)->medtrain
subset(medical, split_tag==F)->medtest
mod_log_medical <- glm(No.show ~ Age + Gender + Diabetes + Alcoholism + Handcap, data=medical_train, family = "binomial") # binary classification 
summary(mod_log_medical)

predict(mod_log_medical,newdata=medtest,type="response")->result_mod_log_medical

head(result_mod_log_medical)  #check

range(result_mod_log_medical)

table(medtest$No.show, result_mod_log_medical)

'''
#Actual Predicted
#   TN      FP
#   FN      TP


   FALSE TRUE
  No    825  469
  Yes    76  391
'''
# TP=391
# FN=76
# TN=825
# FP=469
#Actual Predicted
#   TN      FP
#   FN      TP

(825+391)/1761 # Accuracy   #Accuracy=(TP+TN)/(TP+FP+FN+TN)   #69%
391/(391+76) # Recall       #Recall=TP/(TP+FN)                #83%
391/(391+469) # Precision   #Precision=TP/(TP+FP)             #45%

#Module 06, Assignment 8: AIC value:

#The Akaike information criterion (AIC) is a mathematical method for evaluating how well a model fits the data it was generated from. 
#In statistics, AIC is used to compare different possible models and determine which one is the best fit for the data.

#AIC is a single number score that can be used to determine which of multiple models is most likely to be the best model for a given dataset. 
#It estimates models relatively, meaning that AIC scores are only useful in comparison with other AIC scores for the same dataset. A lower AIC score is better.


#1) 
'''
Call:
glm(formula = Region ~ AvgTemperature + Day + Month + Year, family = "binomial", 
    data = cityt_train)

Deviance Residuals: 
  Min          1Q      Median          3Q         Max  
-6.051e-06  -1.564e-06  -8.873e-07   1.783e-06   6.173e-06  


Null deviance: 8.3153e+02  on 624  degrees of freedom
Residual deviance: 3.8927e-09  on 620  degrees of freedom
AIC: 10

Number of Fisher Scoring iterations: 25
'''
#Conclusion: the model is good as the  AIC is small.

#2)
'''
Call:
  glm(formula = Churn ~ MonthlyCharges + tenure + StreamingTV + 
        TotalCharges, family = "binomial", data = cc_train)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-1.8590  -0.6803  -0.3428   0.7490   3.3419  


Null deviance: 2730.4  on 2369  degrees of freedom
Residual deviance: 2031.1  on 2364  degrees of freedom
(5 observations deleted due to missingness)
AIC: 2043.1
'''
# Conclusion: The model is not good and needs improvement, as AIC should be smaller, not 2043 as it is now.

#3)

'''
Call:
glm(formula = No.show ~ Age + Gender + Diabetes + Alcoholism + 
    Handcap, family = "binomial", data = medical_train)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.7634  -0.6972  -0.6490  -0.5983   2.0353  


(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 79371  on 78946  degrees of freedom
Residual deviance: 79075  on 78941  degrees of freedom
AIC: 79087

Number of Fisher Scoring iterations: 4
'''
#Conclusion: the model in not good, needs much improvement, AIC is 79087 is not reflecting a good performance of the model.

