
#R for Data Science - class August-Sept 2022 with Prateek
#Homework
#Module1, Assignment 01:
#Set up r studio in your pc.
#Print 'x',
#y=2
#z=7
#x=y*z

y<-2
z<-7
x<-y*z
x
#Print using rep function "123123123123", "1111222233334444":

rep(123, 4)

rep(1:3, 4)

rep(1:4, each=4)

#4. Create a vector 'x' and store 21,34,2,5,33,23 in it.
x<-c(21,34,2,5,33,23)
x

#5. Find the level using gl function "112233441122334411223"

gl(4,2, 24)

gl(4,2, 21)  #this is correct answer on $5, truncated version of gl(4,2, 24)


#Module 2 Assignment
#If "a" is a vector consists of "1,3,5,6,3 " and b is another vector where " 3,4,7,2 " is stored in it. 
#write a code so that output will be "1-3" "3-4" "5-7" "6-2" "3-3"

#Create a data frame of two vectors x and y of values "1,4,2,7" and "2,1,7,3" respectively and then find position of 
#maximum value in 'y' .


#Using rbind and cbind form two matrix of vector 'a' and 'b' .a="1,2,3,4,5,6" b="2,3,1,4,3,2".Then find transpose of these matrices.

a="1,2,3,4,5,6" 

b="2,3,1,4,3,2"

#Merge two data frames to find inner join and outer join where first data frame will consist 
#two vectors of random letter and random numbers respectively. 
# 2nd data frame consists of two vector of given letter and a given numbers.letter-a,b,c,d,e.numbers-2,3,4,5,6.

#Using strsplit function split the letters of 2 from sentence "i am good" using '' to show space.

#MOdule 3 Assignment
#Create a matrix using matrix function and then find the mean of each column.


#2. Create a matrix of 5 vectors using cbind method and thus find the mean of rows and columns.
#Create a vector 'a' and find the nth value from the vector.


#4. Print "2,2.1,2.1,2.3,2.4,2.5" using seq function.


#5. Write code using rep function so that output will be-"123451234512345"


#Module 4: Assignment
#1.Plot two vectors “x” and “y” of values (2,4,6,8,10) and(3,2,5,2,8) in a same graph. 
#Limit y-axis to 12 and both the vectors should be displayed in different color and then create a title of that graph “DEMO”. (Points should be connected)

#2.Create a bar plot of number of magazines sold in a week where number of magazines sold in day1=4, day2=6, day3=7, day4=2, day5=6, day6=7, day7=9. 
#X-axis shows days and Y-axis shows total number of magazine sold. Use density to differentiate the bars.


#3.Create a vector ‘a’ and store (10,9,8,7,6,5,4,3,2,1) into it. Access the first four values and then remove the last value from vector. 
#Then display all elements whose value is more than 3.Then finally display all the values which are divisible by 2.


#4.Create a 4-d array with 4 rows and 5 columns with 3 tables and store value from 1 to 40. Display 3 columns.

#5 Create a list of 3 objects consist of bikes model, color and price. Then display each bike model along with its price and color.

#6.6et working directory of files then import a sample csv file.



# Module 5:Assignment

#1.Create a pie chart of number of bikes sold in a year from Jan to dec if no. of bikes sold is 15,22,13,45,11,17,21,33,14,23,22,10 respectively. 
#Calculate percentage of bike sold in each month and display it in different colors.


#2.Create a csv file of 2 groups .value of x in 1st group is 2,5,6,3,7,9,8 and value of x in 2nd group is 3,8,7,6,9.
#Import that csv file in r and analyze whether calculated f value will be accepted or not.


#3.One way analysis and two way analysis of variance of a sample data and create a box plot for both.


#MODULE 6: Assignment

#Create two matrix with 50 rows and 2 columns of random numbers. merge two matrices using rbind function. 
#Display 3 group clusters in a graph and show it in different color.
#Get centroid of each cluster.


#Module 7:   Assignment

#Install package “arules”. Import sample data from this package. Plot a graph of top 10 products from that data(remove all duplicate values).
#Using apriori algorithm, show the product which are min 5% in the data and along with that, show the products belongs to that 50%.
#Finally inspect the rules and get the summary of all quality measures.


#Module 8: Assignment

#Using given sample data, predict the blood pressure of each user which is given in BP.csv and use data from BP_predict.csv. 
#Find the summary of that model and analyze all the functionality.










