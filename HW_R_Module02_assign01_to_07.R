
#R for Data Science - class August-Sept 2022 with Prateek
#Homework
#Module2, Assignment 01
#1.extract from placement datatset:

install.packages("dplyr")  
library(dplyr) 

placement <- read.csv("Placement_Data_Full_Class.csv")
str(placement)
is.data.frame(placement)
View(placement)
#df <- as.data.frame(placement)

#  Assignment 01 Task 1: Filter function

filter(place, hsc_s=="Science" & hsc_b == "Central" & hsc_p>70) -> s_science
View(s_science)

#Assignment 01  Task 2:
filter(place,degree_t=="Comm&Mgmt" & specialisation == "Mkt&Fin") -> d_commerce
View(d_commerce)

#Assignment 01  Task 3:
filter(place,mba_p>75 & etest_p>70)->mba_gt75
View(mba_gt75)

#Assignment 01  Task 4:
filter(place,etest_p>90 & specialisation == "Mkt&Fin") -> mkfin_etest90
View(mkfin_etest90)

#Assignment 01  Task 5:
filter(place, hsc_s=="Science" | hsc_s=="Commerce") -> sci_and_comm
View(sci_and_comm)

#Assignment 2:Task 1:  Median function

customer_churn<- read.csv("Customer_Churn.csv")
cc<- read.csv("Customer_Churn.csv")
str(customer_churn)
is.data.frame(customer_churn)

med_tenure<-median(customer_churn$tenure)
med_tenure

#Assignment 2:Task 2:

med_MonthlyCharges<-median(customer_churn$MonthlyCharges)
med_MonthlyCharges

#Assignment 2:Task 3:

med_TotalCharges<-median(customer_churn$TotalCharges)
med_TotalCharges

#Assignment 2:Task 1b:

med_ssc <- median(placement$ssc_p)
med_ssc

#Assignment 2:Task 2b:

med_hsc <- median(placement$hsc_p)
med_hsc

#Assignment 2:Task 3b:

med_degree<-median(placement$degree_p)
med_degree


#Assignment 3:Task 1a: Mutate function

#Mutate function: create new column and assign the value to it based on condition:
#if senior, assign age between 55 and 100, 
#if not senior - assign 16-55 random.
#a)
mutate(cc,Age=ifelse((SeniorCitizen==1),sample(x=56:100),sample(x=16:55))) -> new_data_age2
View(new_data_age2)
#b)
mutate(cc,Age=ifelse((SeniorCitizen==0),sample(x=16:55),sample(x=56:100))) -> new_data_age3
View(new_data_age3)

#Task 2
mutate(cc,Customer_Category = case_when(MonthlyCharges<45 ~ 'low_paying', 
                                        (MonthlyCharges>46) & (MonthlyCharges<=90) ~ 'medium_paying',
                                        MonthlyCharges >90 ~ 'high_paying')) -> cc_new_paying
View(cc_new_paying)

#Task 3:                            

mutate(cc,Security=ifelse(OnlineSecurity=="Yes", "secure", "not_secure")) -> cc_new_secure
View(cc_new_secure)

#Assignment 4:Task 1: Select function

select(placement,12)->spec_student
View(spec_student)

select(placement,15)->sal_student
View(sal_student)

select(placement,5)->hsc_p_student
View(hsc_p_student)

#Assignment 4:Task 2)

#read pharma dataset pharma_audit_data.csv:
pharma <- read.csv('pharma_audit_data.csv')

select(pharma, c(1,3,5))->cols135_ph
View(cols135_ph)

select(pharma, c(2,6))->cols26_ph
View(cols26_ph)
#3)
select(placement,1:5) -> cols1_to_5_placement
View(cols1_to_5_placement)

#Assignment 5:Task 1-3 cc

sd(cc$tenure)->sd_tenure
sd_tenure

sd(cc$MonthlyCharges)->sd_MonthlyCharges
sd_MonthlyCharges

sd(cc$TotalCharges)->sd_TotalCharges
sd_TotalCharges

#Assignment 5:Task 1-3 placement:

sd(placement$etest_p)->sd_etest
sd_etest

sd(placement$salary)->sd_salary
sd_salary

sd(placement$mba_p)->sd_mba
sd_mba

#Assignment 6:Task 1-5 Summarize function:

summarize(placement, median(etest_p),var(etest_p),sd(etest_p))

summarize(placement, A=median(etest_p),B=var(etest_p),C=sd(etest_p))->summarized_etest_placement

summarized_etest_placement


summarize(placement, median(mba_p),var(mba_p),sd(mba_p))

summarize(placement, A=median(mba_p),B=var(mba_p),C=sd(mba_p))->summarized_mba_placement

summarized_mba_placement

#Assignment 6 #Task3:
#group_by function:

group_by(placement,salary)->g1
View(g1)

group_by(g1,median(hsc_p))->g2
View(g2)

#Assignment 6 #Task4:

group_by(placement,salary)->g3
View(g3)

group_by(g1,var(ssc_p))->g4
View(g4)

#Assignment 6 #Task5:

group_by(placement,salary)->g5
View(g5)

group_by(g1,sd(etest_p))->g6
View(g6)

#Assignment 7 #Task1-3 (cc):  Variance

var(cc$tenure)->var_tenure
var_tenure

var(cc$MonthlyCharges)->var_MonthlyCharges
var_MonthlyCharges

var(cc$TotalCharges)->var_TotalCharges
var_TotalCharges

#Assignment 7 #Task1-3 (placement):  Variance

var(placement$ssc_p)->var_ssc
var_ssc

var(placement$hsc_p)->var_hsc
var_hsc

var(placement$degree_p)->var_degree
var_degree