#R for Data Science - class August-Sept 2022 with Prateek
#Homework
#Module 09, Assignment 01 - 05:  K-means for cc dataset

#Module 09, Assignment 01: Split dataframe:

install.packages("cluster")
install.packages("factoextra")             #Step1 - load libraries and df
library(cluster)
library(factoextra)

df<-read.csv("Customer_churn.csv",stringsAsFactors = T)
#df<-read.csv("Customer_churn.csv")
str(df)
which(is.na(df))

df <- na.omit(df)  #Remove NA
#df <- scale(df)
str(df) # view structure of dataset
summary(df) #view statistical summary of dataset
head(df, n=10) #view top  rows of dataset

customer_features <- df[c(6,19,20)]   # 6 tenure, 19 monthly ch, 20-totalch
df1 <- df[c(6,19)]
str(df1)
glimpse(df1)

#Find optimal number of clusters:        #Step 3 - find opt num of clusters:
fviz_nbclust(df1, kmeans, method = "wss")

#kmeans(df1, centers_number, iter.max = 10, nstart = 1)

#Bumber of clusters vs Gap Statistics:
#calculate gap statistic based on number of clusters
gap_stat <- clusGap(df1,FUN = kmeans,nstart = 25,K.max = 10,B = 50)

#plot number of clusters vs. gap statistic
fviz_gap_stat(gap_stat)

# Compute k-means with k = 4               #Step 4 - preform k-means clustering eith Optimal k:
set.seed(1)
#km <- kmeans(df, 4, nstart = 25)
km <- kmeans(df1, centers=4, nstart = 25)

# Print the results
print(km)

#plot results of final k-means model:
fviz_cluster(km, data=df1)

aggregate(df1, by=list(cluster=km$cluster), mean)

final_data <- cbind(df1, cluster = km$cluster)
head(final_data)
# Cluster number for each of the observations
km$cluster
head(km$cluster, 4)
# Cluster size
km$size
# Cluster means
km$centers

#----------------------------------

set.seed(200)

k_max <- 35  # we will come back to this
k <- 30
#it is kmeans function, it needs 3 arguments: 
#dataset, k number of cluster (our guess), and number of iterations/loops to repozition centroids:

kmeans(df2, k , iter.max = 100)

wss <- sapply(1:k_max,function(k){kmeans(df2,k,iter.max = 100)$tot.withinss}) # WSS
wss

plot(1:k_max,
     wss, 
     type= "b",  # line chart
     xlab = "Number of clusters(k)", 
     ylab = "Within sum of squares")

# K=3

# Apply k-means clustering algorithm

result <- kmeans(df2,3,iter.max = 100) 

View(result)
result$size # gives no. of records in each cluster 
result$centers # gives value of cluster center datapoint value(3 centers for k=3)
result$cluster #gives cluster vector showing the cluster where each record falls
#Verify results of clustering


install.packages("ggplot")
library(ggplot2)

install.packages("gridExtra")
library(gridExtra)

install.packages("dplyr")
library(dplyr)

colnames(df2)[3] <- "clusters"

#Scatter Plot  based on cluster names
plot1 <- ggplot(df2,aes(x = MonthlyCharges, y = tenure, 
                             col= (clusters))) + geom_point()

ggplot(df2,aes(x = MonthlyCharges, y = tenure, 
                    col= (clusters))) + geom_point()


plot2<-ggplot(df2,aes(x = MonthlyCharges, y = tenure, 
                    col= clusters)) + geom_point()

grid.arrange(plot1, plot2, nrow=1, ncol=2)

#__________

plot3 <- ggplot(iris.new,
                aes(x = Petal.Length, y = Petal.Width,col= (clusters))) + geom_point()

ggplot(iris.new,
       aes(x = Petal.Length, y = Petal.Width,col= (clusters))) + geom_point()

plot4 <- ggplot(iris,aes(x = Petal.Length, y = Petal.Width,
                         col= iris.class)) + geom_point()

ggplot(iris,aes(x = Petal.Length, y = Petal.Width,
                col= iris.class)) + geom_point()

grid.arrange(plot3, plot4, ncol=2)
grid.arrange(plot1, plot2, plot3, plot4, ncol=2, nrow=2)

par(mfrow=c(1,2)) #fitting multiple plots in a view

plot(iris.new[c(1,2)], col=result$cluster) # Plot to see how Sepal.Length and Sepal.Width data points have been distributed in clusters
plot(iris.new[c(1,2)], col=iris.class)# Plot to see how Sepal.Length and Sepal.Width data points have been distributed originally as per "class" attribute in dataset

plot(iris.new[c(3,4)], col=result$cluster)# Plot to see how Petal.Length and Petal.Width data points have been distributed in clusters
plot(iris.new[c(3,4)], col=iris.class)





