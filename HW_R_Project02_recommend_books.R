#Project 02 - Books Recommendation System:

#install.packages("recommenderlab")
library("dplyr")  # data manipulation
#install.packages("tidyr")
library("tidyr") # data manipulation
install.packages("Matrix")
library("Matrix") # this is convert different matrix structures
#install.packages("recommenderlab")
library("recommenderlab") # building the reccomendor system engine

books<-read.csv("books.csv",stringsAsFactors = T)
ratings<-read.csv("ratings_1.csv",stringsAsFactors = T)

#we transform ratings dataframe into format for User-based-Recommendation: (transpose)

dimension_names<-list(user_id=sort(unique(ratings$user_id)),  # list of all unique userid in the rating dataset
                      book_id=sort(unique(ratings$book_id)))  # list of all unique book ids


rating_mat<-spread(ratings,book_id,rating) # Tidyr , transpose of data

View((rating_mat[,1:30]))

rating_mat<-select(rating_mat,-user_id)  # remove user id field from rating_mat object

View(rating_mat[,1:30])

#------------#

rating_mat<-as.matrix(rating_mat) # convert to matrix ; homogenous - all values should be of same datatype

class(rating_mat)

View(rating_mat[1:30,1:30]) #view first 30 rows and 30 columns

dimnames(rating_mat)<-dimension_names #used to assign rownames and colnames to a matrix object

(rating_mat[1:10,1:10])

dim(rating_mat)  #all rows represented by user id and all columns represented by book id

#--------------------------------------------------------------------------
  
#Converting the simple matrix into a real rating matrix:

rating_mat0<-rating_mat  #we create duplicate object (back up to be available for the case of error)

dim(rating_mat0)

View(rating_mat0 [1:10,1:10])

rating_mat0[is.na(rating_mat0)]<-0 #To convert all missing values to 0

View(rating_mat0 [1:10,1:10])

# Sparse Matrix: compressedformat, saves a lot of space

sparse_ratings<-as(rating_mat0,"sparseMatrix") 

# converts matrix into sparse matrix, saves space and compresses data

sparse_ratings[1:15,1:15]

#Last part of out transformation

real_ratings<-new("realRatingMatrix", data=sparse_ratings)

real_ratings
#----------------------------------------------------------#

# 5 recommendations of books for each user in my test data 

#Split data into train and test data : 80:20 split

split_tag<- sample(c(T,F),size=nrow(real_ratings),replace=T, prob = c(0.8,0.2))   #80 -20 percent train to test data

recc_train<-real_ratings[split_tag,] 

recc_test<-real_ratings[!split_tag,] #Split tag is ! True

#------------------------------------------------------------------------------------
  
#Building the ubcf model : USER BASED COLLABORATIVE FILTERING MODEL
  
recc_model_ubcf <- Recommender(data = recc_train, 
                               method="UBCF") # USER/ITEM BASED COLL FILTERING

#method = IBCF - Item based collab filterting
#method = UBCF - User based collab filtering

n_rec<-10 #number of reccommendations to be generated for each user

# Recommending books for user number 5:

recc_predicted_ubcf<-predict(object=recc_model_ubcf,newdata=recc_test[5], n=n_rec)

#Extracting book labels for predicted model:

Top_10_List = as(recc_predicted_ubcf, "list")
Top_10_List
Top_10_df=data.frame(Top_10_List)
colnames(Top_10_df)="book_id"
Top_10_df$book_id<-as.numeric(as.character(Top_10_df$book_id))

names=left_join(Top_10_df, select(books,book_id,authors,title,language_code,average_rating), by="book_id")

View(names)

#--------------------------------------------------------------

#Building the IBCF model : ITEM BASED COLLABORATIVE FILTERING MODEL:
recc_model_ibcf <- Recommender(data = recc_train, method="IBCF") # USER/ITEM BASED COLL FILTERING

n_rec<-10 #number of reccommendations to be generated for each user

#Recommending books for user number 5:

recc_predicted_ibcf<-predict(object=recc_model_ibcf,newdata=recc_test[5], n=n_rec)

#Extracting book labels for predicted model

Top_10ibcf_List = as(recc_predicted_ibcf, "list")
Top_10ibcf_List
Top_10ibcf_df=data.frame(Top_10ibcf_List)
colnames(Top_10ibcf_df)="book_id"
Top_10ibcf_df$book_id<-as.numeric(as.character(Top_10ibcf_df$book_id))

names_ibcf=left_join(Top_10ibcf_df, select(books,book_id,authors,title,language_code,average_rating), by="book_id")
View(names_ibcf)
