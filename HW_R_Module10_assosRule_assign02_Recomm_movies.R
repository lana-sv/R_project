#R for Data Science - class August-Sept 2022 with Prateek
#Homework

#Module 10, Assignment 02: Recomm for movies:
#1)  load movielense dataset (movies.csv and ratings.scv)
#2) extract only info where users seen at least 100 movies and each movie has been seen at least 120 times.Store result in 'sample_movie'
#3) divide samle_movie into train and test set. Splot 70 to 30
#4) build User Based Collaborative Filtering model on train set and store result in ubcf_model
#5) predict the values on the test set. Number of recommended movie = 10
#6)recommend movies for user3 and user 5

install.packages("recommenderlab")
library("dplyr")  # data manipulation
#install.packages("tidyr")
library("tidyr") # data manipulation
install.packages("Matrix")
library("Matrix") # this is convert different matrix structures
#install.packages("recommenderlab")
library("recommenderlab") # building the reccomendor system engine
library(ggplot2)                      
library(data.table) 
library(reshape2)

movies<-read.csv("movies.csv",stringsAsFactors = T)
head(movies)
summary(movies)

movies <- tbl_df(movies) #Please use `tibble::as_tibble()` instead.
movies <- as_tibble(movies)
head(movies)


ratings<-read.csv("ratings.csv",stringsAsFactors = T)
#ratings2 <- ratings[1:100000,]
#str(ratings2)
ratings2 <- ratings[,c(1,2,3)]
str(ratings2)
summary(ratings2)

ratings2 <- tbl_df(ratings2) #Please use `tibble::as_tibble()` instead.
ratings2 <- as_tibble(ratings2) 
head(ratings2)




merged_data <- inner_join(movies, ratings2, by="movieId")
head(merged_data)
colSums(is.na(merged_data))
merged_data <- as_tibble(merged_data) 
group_by(merged_data, by='Title')
summarize(merged_data)
summary(merged_data)

merged_data  %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) 
  
merged_data  %>%
  group_by(userId) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
  
merged_data  %>%
  group_by(movieId)  %>%
summarize(count = n()) %>%
  arrange(desc(count))
  
merged_data  %>%
  group_by(title())  %>%
  summarize(count = n())  %>%
  arrange(desc(count))

merged_data %>%
  group_by(group = cut(userId, breaks = seq(0, max(userId), 1000))) %>%
  summarise(n = n())

movies_distinct<- distinct(merged_data,movieId, .keep_all = TRUE)
movies_distinct

movies_subset <- movies_distinct %>% filter(rating >= "4", genres != "Adult") 
movies_subset

colSums(is.na(movies_subset))

movies_subset$movie_rating %>%  boxplot(main="Box Plot of Movie Ratings", ylab="rating", col = "pink")

filter(merged_data, title == "Toy Story (1995)")

filter(merged_data, rating <= 1)

filter(merged_data, title == "Fight Club (1999)", rating <= 1)

merged_data %>%
  filter(movieId == 1) %>%
  summarize(mean(rating))


merged_data %>%
  filter(rating == 5) %>%
  select(title) %>%
  distinct() %>%
  head(n=10)

su_ar<-merged_data %>%
  group_by(movieId) %>%
  summarize(su = sum(userId), count = n())%>%
  arrange(desc(count))
  
  
su<-merged_data %>%
  group_by(movieId) %>%
  summarize(su = sum(userId), count = n())

tail(su_ar,20)
head(su_ar, 20)
tail(su_ar[1:8000,])

tail(su_ar[1:3000,])

tail(su_ar[1:2100,])

#100 users watched the movies:
users100<-(su_ar[1:2100,])
head(users100)

users100$movieId

movies100=left_join(users100, select(movies, movieId,title,genres), by="movieId")
str(movies100)

movies100 <- movies100[,c(1,4,5)]
str(movies100)

ratings100=left_join(users100, select(ratings2, userId, movieId,rating), by="movieId")
str(ratings100)

ratings100 <- ratings100[,c(1,4,5)]
str(ratings100)

#we transform ratings dataframe into format for User-based-Recommendation: (transpose)

dimension_names<-list(userId=sort(unique(ratings100$userId)),  # list of all unique userid in the rating dataset
                      movieId=sort(unique(ratings100$movieId)))  # list of all unique book ids


rating_mat<-spread(ratings100,movieId,rating) # Tidyr , transpose of data

View((rating_mat[,1:30]))
str(rating_mat)
#rating_mat<-select(rating_mat, -userId)  # remove user id field from rating_mat object
rating_mat2 <- rating_mat[,c(2:2101)]
str(rating_mat2)

View(rating_mat2[,1:30])
rnames<-rating_mat2[,]
#rnames
#------------#

rating_mat<-as.matrix(rating_mat2) # convert to matrix ; homogenous - all values should be of same datatype

class(rating_mat)

View(rating_mat[1:30,1:30]) #view first 30 rows and 30 columns

dimnames(rating_mat)<-dimension_names #used to assign rownames and colnames to a matrix object
rating_mat[1:10,1:10]

dim(rating_mat)  #all rows represented by user id and all columns represented by movie id

#--------------------------------------------------------------------------

#Converting the simple matrix into a real rating matrix:

rating_mat0<-rating_mat  #we create duplicate object (back up to be available for the case of error)

dim(rating_mat0)

View(rating_mat0 [1:10,1:10])

rating_mat0[is.na(rating_mat0)]<-0 #To convert all missing values to 0

View(rating_mat0[1:10,1:10])

# Sparse Matrix: compressedformat, saves a lot of space

sparse_ratings<-as(rating_mat0,"sparseMatrix") 

# converts matrix into sparse matrix, saves space and compresses data

sparse_ratings[1:15,1:15]

#Last part of out transformation

real_ratings<-new("realRatingMatrix", 
                  data=sparse_ratings)

real_ratings
#----------------------------------------------------------#

# 5 recommendations of movies for each user in my test data 

#Split data into train and test data : 80:20 split

split_tag<- sample(c(T,F),
                   size=nrow(real_ratings),
                   replace=T, 
                   prob = c(0.8,0.2))   #80 -20 percent train to test data

recc_train<-real_ratings[split_tag,] 

recc_test<-real_ratings[!split_tag,] #Split tag is ! True

#------------------------------------------------------------------------------------

#Building the ubcf model : USER BASED COLLABORATIVE FILTERING MODEL

recc_model_ubcf <- Recommender(data = recc_train, 
                               method="UBCF") # USER/ITEM BASED COLL FILTERING

#method = IBCF - Item based collab filterting
#method = UBCF - User based collab filtering

n_rec<-10 #number of reccommendations to be generated for each user

# Recommending books for user no 5:

recc_predicted_ubcf<-predict(object=recc_model_ubcf,  
                             newdata=recc_test[5], # USER NUMBER = 5 ;
                             n=n_rec)

#Extracting book labels for predicted model:

Top_10_List = as(recc_predicted_ubcf, "list")
Top_10_List
Top_10_df=data.frame(Top_10_List)
colnames(Top_10_df)="movieId"
Top_10_df$movieId<-as.numeric(as.character(Top_10_df$movieId))

names=left_join(Top_10_df, select(movies, movieId,title,genres), by="movieId")

View(names)

#-------------------
# Recommending books for user no 3:

recc_predicted_ubcf<-predict(object=recc_model_ubcf,  
                             newdata=recc_test[3], # USER NUMBER = 3 ;
                             n=n_rec)

#Extracting book labels for predicted model:

Top_10_List = as(recc_predicted_ubcf, "list")
Top_10_List
Top_10_df=data.frame(Top_10_List)
colnames(Top_10_df)="movieId"
Top_10_df$movieId<-as.numeric(as.character(Top_10_df$movieId))

names=left_join(Top_10_df, select(movies, movieId,title,genres), by="movieId")

View(names)

#--------------------------------------------------------------

#Building the IBCF model : ITEM BASED COLLABORATIVE FILTERING MODEL:

recc_model_ibcf <- Recommender(data = recc_train, method="IBCF") # USER/ITEM BASED COLL FILTERING

n_rec<-10 #number of reccommendations to be generated for each user

#Recommending movies for user no 2 :

recc_predicted_ibcf<-predict(object=recc_model_ibcf,
                             newdata=recc_test[2], # USER NO - 2
                             n=n_rec)

#Extracting book labels for predicted model

Top_10ibcf_List = as(recc_predicted_ibcf, "list")
Top_10ibcf_List
Top_10ibcf_df=data.frame(Top_10ibcf_List)
colnames(Top_10ibcf_df)="movieId"
Top_10ibcf_df$book_id<-as.numeric(as.character(Top_10ibcf_df$movieId))

names_ibcf=left_join(Top_10ibcf_df, select(movies,movieId,title,genres), by="movieId")
View(names_ibcf)

#------------
#Recommending movies for user no 4 :

recc_predicted_ibcf<-predict(object=recc_model_ibcf,
                             newdata=recc_test[4], # USER NO - 4
                             n=n_rec)

#Extracting book labels for predicted model

Top_10ibcf_List = as(recc_predicted_ibcf, "list")
Top_10ibcf_List
Top_10ibcf_df=data.frame(Top_10ibcf_List)
colnames(Top_10ibcf_df)="movieId"
Top_10ibcf_df$book_id<-as.numeric(as.character(Top_10ibcf_df$movieId))

names_ibcf=left_join(Top_10ibcf_df, select(movies,movieId,title,genres), by="movieId")
View(names_ibcf)






