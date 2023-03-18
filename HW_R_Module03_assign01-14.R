#R for Data Science - class August-Sept 2022 with Prateek
#Homework
#Module3, Assignment 01 aesthetic
#Task1.
install.packages("ggplot2")  #library to build visualization - Grammer of Graphics Plot2
install.packages("plotly")
#library(dplyr)

library(ggplot2) # Activate the library
library(plotly)

customer_churn<- read.csv("/Users/lana-n/datacsv/Customer_Churn.csv")
cc<- read.csv("/Users/lana-n/datacsv/Customer_Churn.csv")
str(customer_churn)

#data - dataframe used to build visualization
#aesthetic - variables used to build the plots
#geometry - histogram, barplot, scatter
#facet (optional) - divide/create sub-segments on data and build visualizations of it - optional component
#Histograms  : to depicked frequency distribution of a continuous

#a)
ggplot(data = customer_churn, aes(x=PhoneService)) + geom_bar()

#b)
ggplot(data = customer_churn, aes(x=gender)) + geom_bar()

#c)
ggplot(data = customer_churn,aes(x=InternetService,fill=MonthlyCharges))+geom_bar()

ggplot(data = customer_churn,aes(x=InternetService,y=MonthlyCharges))+geom_boxplot()

#d) 
ggplot(data = customer_churn,aes(x=MonthlyCharges,fill=SeniorCitizen))+geom_bar()

ggplot(data = customer_churn,aes(x=MonthlyCharges,y=SeniorCitizen))+geom_boxplot()

#e) 
ggplot(data = customer_churn,aes(x=TotalCharges,fill=tenure))+geom_bar()

ggplot(data = customer_churn,aes(x=TotalCharges,fill=tenure))+geom_boxplot()

ggplot(data = customer_churn,aes(x=TotalCharges, y=tenure))+geom_boxplot()

#f)
ggplot(data = customer_churn,aes(x=StreamingTV,fill=SeniorCitizen))+geom_bar()

#g)
ggplot(data = customer_churn,aes(x=Dependents,fill=Partner))+geom_bar()

ggplot(data = customer_churn, aes(x=tenure)) + geom_histogram()

ggplot(data = customer_churn, aes(x=tenure)) + geom_bar()

ggplot(data = customer_churn, aes(x=tenure)) + geom_scatter()

#Module3, Assignment 02 bins

pharma <- read.csv('/Users/lana-n/datacsv/pharma_audit_data.csv')
str(pharma)

# I did not found the dataset for this task in the downloaded folder,
#so I just downloaded similar dataset from kaggle using the following link:

# https://www.kaggle.com/datasets/joniarroba/noshowappointments?resource=download

#it does not have Issues column but instead has diabetes column which I can use
#it does not have Drug_id  column but instead has AppointmentID column which I can use
# it has PatientID,Gender and Age columns  
medical<- read.csv("/Users/lana-n/datacsv/medical.csv")
str(medical)
#change number of bins and add color

# 72 months is longest tenure, it is 6 years. We choose 6 bins/years

ggplot(data = medical, 
       aes(x=Age)) + 
  geom_histogram(bins = 8, #number of breaks/bars
                 fill= "green", # Color of bars
                 col= "blue") # Color of border


ggplot(data = medical, 
       aes(x=Age)) + 
  geom_histogram(bins = 8, #number of breaks/bars
                 fill= "white", # Color of bars
                 col= "azure") # Color of border

#Task2:
ggplot(data = medical, 
       aes(x=PatientId)) + 
  geom_histogram(bins = 50, #number of breaks/bars
                 fill= "black", # Color of bars
                 col= "wheat3") # Color of border

#Task3:

ggplot(data = customer_churn,aes(x=MonthlyCharges))+
  geom_histogram(bins=80,fill="black", col="violet")

#Task4:

ggplot(data = customer_churn,aes(x=tenure))+
  geom_histogram(bins=50,fill="black", col="white")


#Module3, Assignment 03 col

placement <- read.csv("/Users/lana-n/datacsv/Placement_Data_Full_Class.csv")
str(placement)
#a)
ggplot(data = placement,
       aes(y=ssc_p,x=gender,col=ssc_b))+
  geom_point() 

#b)
ggplot(data = placement,
       aes(y=hsc_p,x=ssc_b,col=degree_t))+
  geom_point() 

#c)
ggplot(data = placement,
       aes(y=salary,x=status,col=specialisation))+
  geom_point()

#d)
ggplot(data = placement,
       aes(y=etest_p,x=degree_t,col=workex))+
  geom_point()

#e)
ggplot(data = placement,
       aes(y=hsc_b,x=hsc_s,col=gender))+
  geom_point()

#f)

ggplot(data = placement,
       aes(y=etest_p,x=hsc_s,col=degree_t))+
  geom_point()

#Module3, Assignment 04 facet

#1)
ggplot(data = customer_churn, aes(y=tenure,x=TotalCharges,col=gender)) + geom_point() + facet_grid(~gender)

#2)
ggplot(data = customer_churn, aes(x=InternetService,y=tenure,col=Contract)) + geom_point() + facet_grid(~Contract)

#3)
ggplot(data = placement, aes(x=mba_p,y=etest_p,col=degree_t)) + geom_point() + facet_grid(~degree_t)

#Module3, Assignment 05 fill:

#barplots:
#1)
counts <- table(placement$etest_p, placement$specialisation)
barplot(counts)

barplot(counts, horiz=TRUE)

barplot(counts, main="Simple barplot", xlab='etest_p', ylab='specialisation', legend=rownames(counts), col=c("red", "yellow","green"))
#2)
counts2 <- table(placement$degree_t)
barplot(counts2)
barplot(counts2, main="Simple barplot degree_t", xlab='spec', ylab='degree_t', legend=rownames(counts2), col=c("orange", "peru"))

#3)
counts3 <- table(placement$hsc_s)
barplot(counts3, main="Simple barplot hsc_s", xlab='spec', ylab='hsc_s', legend=rownames(counts3), col=c("grey", "cornflowerblue"))

#Module3, Assignment 06 geom_bar:

pharma <- read.csv('pharma_audit_data.csv')
str(pharma)
#1)
counts5 <- table(pharma$LOCATION_ID)
barplot(counts5)

barplot(counts5, horiz=TRUE)

barplot(counts5, main="LOCATION_ID barplot", xlab='x-axis', ylab='y-axis', legend=rownames(counts5), col=c("red", "yellow","green"))

#2) #do not see the drug_id column in parma dataset. Will use medical dataset instead.

medical<- read.csv("medical.csv")
str(medical)

counts6 <- table(medical$PatientId)
barplot(counts6)

barplot(counts6, horiz=TRUE)

barplot(counts6, main="PatientId barplot", xlab='x-axis', ylab='y-axis', legend=rownames(counts6), col=c("orange", "peru"))

#3)
counts7 <- table(medical$Age)
barplot(counts7, horiz=TRUE)

barplot(counts7, main="gender barplot", xlab='x-axis', ylab='y-axis', legend=rownames(counts7), col=c("orange", "peru"))

#Module3, Assignment 07 geom_point:
#1)
ggplot(data = placement,aes(y=ssc_p,x=hsc_p, col=gender))+geom_point()

ggplot(data = placement,aes(y=ssc_p,x=hsc_p, col=status))+geom_point()

#2)
ggplot(data = placement,aes(y=mba_p,x=degree_p,col=status))+geom_point() 

ggplot(data = placement,aes(y=mba_p,x=degree_p,col=specialisation))+geom_point() 

#3)
ggplot(data = placement,aes(y=salary,x=etest_p,col=workex))+geom_point() 

ggplot(data = placement,aes(y=salary,x=etest_p,col=gender))+geom_point() 

#Module3, Assignment 08 theme():
#1)
g1<-ggplot(data = placement,aes(x=specialisation,fill=gender))+geom_bar()
g1 + labs(title = "placement specialisation")
g2<-g1 + labs(title = "placement specialisation")
#Adding theme or panel background
g2 + theme(panel.background = element_rect(fill = "skyblue"))  
g3 <- g2+ theme(panel.background = element_rect(fill = "skyblue"))  
g3
#Adding plot background
g4<-g3+theme(plot.background = element_rect(fill = "green"))
g4
#Adjusting title position and color/appearance
g4+theme(plot.title = element_text(hjust = 0.5, face="bold", colour = "black"))
ggplot(data = placement,aes(x=specialisation,fill=gender))+geom_bar()

#2)
gg1<-ggplot(data = placement,aes(y=hsc_p,x=ssc_p))+geom_point()

gg1 + labs(title = "placement hsc_P and ssc_p")
gg2<-gg1 + labs(title = "placement hsc_P and ssc_p")
#Adding theme or panel background
gg2 + theme(panel.background = element_rect(fill = "skyblue"))  
gg3 <- gg2+ theme(panel.background = element_rect(fill = "skyblue"))  
gg3
#Adding plot background
gg4<-gg3+theme(plot.background = element_rect(fill = "beige"))
gg4
#Adjusting title position and color/appearance
gg4+theme(plot.title = element_text(hjust = 0.5, face="bold", colour = "lightgreen"))
ggplot(data = placement,aes(x=specialisation,fill=gender))+geom_bar()


#Module3, Assignment 09 xlab, ylab

#1)plot(customer_churn$InternetService)  

plot(placement$hsc_p, 
     col=c("orange","red"), # add colors to bars
     ylim=c(0,200), # range of y-axis
     xlab="higher secondary subjects", #title of x-axis
     ylab="# Customers", # title of y-axis
     main="Placement dataset - higher secondary subjects")

#2)

plot(placement$degree_p, 
     col=c("palegreen4","red"), # add colors to bars
     ylim=c(0,100), # range of y-axis
     xlab="percentage in graduation", #title of x-axis
     ylab="# number of students", # title of y-axis
     main="Placement dataset - degree_p")

#3)

plot(placement$specialisation)
     
plot(placement$specialisation, 
     col=c("palegreen4","red"), # add colors to bars
     ylim=c(0,100), # range of y-axis
     xlab="specialisation field", #title of x-axis
     ylab="# number of students", # title of y-axis
     main="Placement dataset - specialisation")

plot(placement$etest_p, 
     col=c("palegreen4","red"), # add colors to bars
     ylim=c(0,100), # range of y-axis
     xlab="etest", #title of x-axis
     ylab="# number of students", # title of y-axis
     main="Placement dataset - etest")


#Module3, Assignment 10 geom_boxplot
#1)
boxplot(placement$etest_p,labels=T)

boxplot(placement$etest_p ~ placement$workex)

boxplot(placement$etest_p ~ placement$workex, col = c("wheat3"))

boxplot(placement$etest_p ~ placement$workex,col = c("wheat3"), main = "Distribution etest_p and workex",xlab = "workex", ylab = "etest_p")

#2)
boxplot(placement$etest_p ~ placement$gender)

boxplot(placement$etest_p ~ placement$gender, col = c("snow3"))

boxplot(placement$etest_p ~ placement$workex,col = c("snow3"), main = "Distribution etest_p and gender",xlab = "gender", ylab = "etest_p")

#3)
boxplot(placement$etest_p ~ placement$specialisation, col = c("lightgreen"))

boxplot(placement$etest_p ~ placement$specialisation,col = c("lightgreen"), main = "Distribution etest_p and specialisation",xlab = "specialisation", ylab = "etest_p")

#3)
#Module3, Assignment 11 geom_histogramm

#1)
ggplot(data = placement, aes(x=ssc_p)) + geom_histogram(bins = 50, fill= "cornsilk4", col= "blue", main = "SSC Percentage")

#2)
ggplot(data = placement, aes(x=hsc_p)) + geom_histogram(bins = 50, fill= "black", col= "blue", main = "HSC Percentage")

#3)
ggplot(data = placement, aes(x=degree_p)) + geom_histogram(bins = 80, fill= "white", col= "violet", main = "Degree Percentage")

#4)
ggplot(data = placement, aes(x=etest_p)) + geom_histogram(bins = 100, fill= "black", col= "white", main = "E-test Percentage")

#Module3, Assignment 12 Labs_function()
#1)
ggplot(data = placement,aes(x=degree_t))+geom_bar(col='blue',fill='yellowgreen') + labs(title = "Degree Plot")

#2)
ggplot(data = placement,aes(x=ssc_b))+geom_bar(col='black',fill=c('beige')) + labs(title = "SSC Plot")
#3)

ggplot(data = placement,aes(x=hsc_b))+geom_bar(col='black',fill=c('cornsilk4')) + labs(title = "HSC Plot")
#4)

ggplot(data = placement,aes(x=specialisation))+geom_bar(col='black',fill=c('pink')) + labs(title ="Specialisation Plot")


#Module3, Assignment 13 Plot() function:

# density plots:
#1)
density_data <- density(placement$degree_p)
plot(density_data)
plot(density_data, main='degree_p')
polygon(density_data, col='skyblue', border='black')

#2)
density_data2 <- density(placement$hsc_p)
plot(density_data2)
plot(density_data2, main='hsc_p')
#3)
spec <- c("Mkt&Fin", "Mkt&HR")
plot(spec, placement$specialisation, type='b', main="title of the graph")
placement$specialisation
plot(months, weight_vec,type='b', main="title of the graph", col='red')

#4)
density_data3 <- density(placement$hsc_p)
plot(density_data3)
plot(density_data3, main='hsc_p')
polygon(density_data3, col='lightgreen', border='black')

#Module3, Assignment 14 Plotly() function
#library(ggplot2) # Activate the library
#library(plotly)
ggplot(data = medical, aes(x=Age, fill=Gender)) + geom_histogram()

ggplot(data = medical, aes(x=Age, fill=No.show)) + geom_histogram()

ggplot(data = medical,aes(y=Age)) + geom_boxplot(fill="green")

ggplot(data = medical,aes(y=Gender)) + geom_boxplot(fill="green")

#2)
ggplot(data = medical, aes(x=Age, fill=Diabetes)) + geom_histogram()

ggplot(data = medical,
       aes(y=Age,x=Gender, fill=Diabetes))+geom_point() # Scatter plot
#3)
ggplot(data = medical,aes(y=Age,x=AppointmentID, fill=Gender))+geom_boxplot()
