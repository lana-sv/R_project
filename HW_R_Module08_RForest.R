
#R for Data Science - class August-Sept 2022 with Prateek
#Homework
#Module 08, Assignment 01 - 10:  RForest for placement dataset, cc, pharma.

#Module 08, Assignment 01: Accuracy for city_temp dataset, cchurn, pharma:
data()


#Instead of city_temp, c_churn and pharma datasets I will use loan and bank datasets that I downloaded today from Kaggle:

#https://www.kaggle.com/datasets/vikasukani/loan-eligible-dataset?select=loan-test.csv taken from here Loan Eligible dataset

loan<- read.csv("loan-train.csv")   #614 records
str(loan)
print(head(loan))
#loan<- read.csv("loan-train.csv", stringsAsFactors=T)

# https://www.kaggle.com/datasets/henriqueyamahata/bank-marketing

bank<- read.csv("bank-additional-full.csv", sep = ';')   #41188 records
str(bank)
print(head(bank))
View(bank)


#It is only because the city_temp dataset is not showing good results for decision tree model, that does not allow to build proper random forest model on the top of it (with feature importance).
#And pharma dataset that I have does not have right columns (DrugId column is not present).

#Please see 6 attached files:

# r05_decision_tree_bank.R
# r05_decision_tree_cchurn.R
# r05_decision_tree_loan.R

#and 

# r05_rforest_bank.R
# r05_rforest_cchurn.R
# r05_rforest_loan.R
