#module 13 - AI. SVM (Support Vector Machine):

#Breast Diagnostics:

#dataset is downloaded from the link below:

#https://www.kaggle.com/code/mirichoi0218/classification-breast-cancer-or-not-with-15-ml/data?select=data.csv

wbcd <- read.csv("/Users/lana-n/datacsv/data_breast_wisconsin.csv", header=T, stringsAsFactors=F)

wbcd$X <- NULL

wbcd <- wbcd[,-1]
wbcd$diagnosis <- factor(ifelse(wbcd$diagnosis=="B","Benign","Malignant"))

str(wbcd)

summary(wbcd)

head(wbcd)
knitr::kable(head(wbcd))


#mean:
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(wbcd[,c(2:11)],histogram=TRUE, col="grey10", pch=1, main="Cancer Mean")


#SE:
install.packages("psych")
library(psych)
pairs.panels(wbcd[,c(12:21)], method="pearson",
             hist.col = "#1fbbfa", density=TRUE, ellipses=TRUE, show.points = TRUE,
             pch=1, lm=TRUE, cex.cor=1, smoother=F, stars = T, main="Cancer SE")

install.packages("ggplot2")
install.packages("GGally")
library(ggplot2)
library(GGally)
ggpairs(wbcd[,c(22:31)],)+ theme_bw()+
  labs(title="Cancer Worst")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=13))

#the relation between each variables (diagnosis included):
library(ggplot2)
library(GGally)

#mean:
ggpairs(wbcd[,c(2:11,1)], aes(color=diagnosis, alpha=0.75), lower=list(continuous="smooth"))+ theme_bw()+
  labs(title="Cancer Mean")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))

#SE:
ggpairs(wbcd[,c(12:21,1)], aes(color=diagnosis, alpha=0.75), lower=list(continuous="smooth"))+ theme_bw()+
  labs(title="Cancer SE")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))

#worst:
ggpairs(wbcd[,c(22:31,1)], aes(color=diagnosis, alpha=0.75), lower=list(continuous="smooth"))+ theme_bw()+
  labs(title="Cancer Worst")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))


#let's see the ggcorr plot:
#mean:
ggcorr(wbcd[,c(2:11)], name = "corr", label = TRUE)+
  theme(legend.position="none")+
  labs(title="Cancer Mean")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))

#SE:
ggcorr(wbcd[,c(12:21)], name = "corr", label = TRUE)+
  theme(legend.position="none")+
  labs(title="Cancer SE")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))
#worst:
ggcorr(wbcd[,c(22:31)], name = "corr", label = TRUE)+
  theme(legend.position="none")+
  labs(title="Cancer Worst")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))


#Principal Component Analysis (PCA):

library(factoextra)
wbcd_pca <- transform(wbcd) 

all_pca <- prcomp(wbcd_pca[,-1], cor=TRUE, scale = TRUE)
summary(all_pca)

mean_pca <- prcomp(wbcd_pca[,c(2:11)], scale = TRUE)
summary(mean_pca)

se_pca <- prcomp(wbcd_pca[,c(12:21)], scale = TRUE)
summary(se_pca)

worst_pca <- prcomp(wbcd_pca[,c(22:31)], scale = TRUE)
summary(worst_pca)


#Screeplot:

fviz_eig(all_pca, addlabels=TRUE, ylim=c(0,60), geom = c("bar", "line"), barfill = "pink", barcolor="grey",linecolor = "red", ncp=10)+
  labs(title = "Cancer All Variances - PCA",
       x = "Principal Components", y = "% of variances")


fviz_eig(mean_pca, addlabels=TRUE, ylim=c(0,60), geom = c("bar", "line"), barfill = "pink", barcolor="grey",linecolor = "red", ncp=10)+
  labs(title = "Cancer Mean Variances - PCA",
       x = "Principal Components", y = "% of variances")


fviz_eig(se_pca, addlabels=TRUE, ylim=c(0,60), geom = c("bar", "line"), barfill = "pink", barcolor="grey",linecolor = "red", ncp=10)+
  labs(title = "Cancer SE Variances - PCA",
       x = "Principal Components", y = "% of variances")

fviz_eig(worst_pca, addlabels=TRUE, ylim=c(0,60), geom = c("bar", "line"), barfill = "pink", barcolor="grey",linecolor = "red", ncp=10)+
  labs(title = "Cancer Worst Variances - PCA",
       x = "Principal Components", y = "% of variances")

#Get PCA Variables:

all_var <- get_pca_var(all_pca)
all_var

mean_var <- get_pca_var(mean_pca)
mean_var

se_var <- get_pca_var(se_pca)
se_var

worst_var <- get_pca_var(worst_pca)
worst_var

#Correlation between variables and PCA:
library("corrplot")
corrplot(worst_var$cos2, is.corr=FALSE)

#Contributions of variables to PCA
#To highlight the most contributing variables for each components:

corrplot(worst_var$contrib, is.corr=FALSE)  


#Contributions of variables to PC1 & PC2:
install.packages("gridExtra")
library(gridExtra)
p1 <- fviz_contrib(worst_pca, choice="var", axes=1, fill="pink", color="grey", top=10)
p2 <- fviz_contrib(worst_pca, choice="var", axes=2, fill="skyblue", color="grey", top=10)
grid.arrange(p1,p2,ncol=2)


#plot - color variables by groups:
#all:
set.seed(218)
res.all <- kmeans(all_var$coord, centers = 6, nstart = 25)
grp <- as.factor(res.all$cluster)

fviz_pca_var(all_pca, col.var = grp, 
             palette = "jco",
             legend.title = "Cluster")


#mean:
set.seed(218)
res.mean <- kmeans(mean_var$coord, centers = 3, nstart = 25)
grp <- as.factor(res.mean$cluster)

fviz_pca_var(mean_pca, col.var = grp, 
             palette = "jco",
             legend.title = "Cluster")


#SE:
set.seed(218)
res.se <- kmeans(se_var$coord, centers = 4, nstart = 25)
grp <- as.factor(res.se$cluster)

fviz_pca_var(se_pca, col.var = grp, 
             palette = "jco",
             legend.title = "Cluster")


#worst:
set.seed(218)
res.worst <- kmeans(worst_var$coord, centers = 3, nstart = 25)
grp <- as.factor(res.worst$cluster)

fviz_pca_var(worst_pca, col.var = grp, 
             palette = "jco",
             legend.title = "Cluster")


#the Biplot:

library("factoextra")


#all:
fviz_pca_biplot(all_pca, col.ind = wbcd$diagnosis, col="black",
                palette = "jco", geom = "point", repel=TRUE,
                legend.title="Diagnosis", addEllipses = TRUE)

#mean:

fviz_pca_biplot(mean_pca, col.ind = wbcd$diagnosis, col="black",
                palette = "jco", geom = "point", repel=TRUE,
                legend.title="Diagnosis", addEllipses = TRUE)
#SE:
fviz_pca_biplot(se_pca, col.ind = wbcd$diagnosis, col="black",
                palette = "jco", geom = "point", repel=TRUE,
                legend.title="Diagnosis", addEllipses = TRUE)

#worst:

fviz_pca_biplot(worst_pca, col.ind = wbcd$diagnosis, col="black",
                palette = "jco", geom = "point", repel=TRUE,
                legend.title="Diagnosis", addEllipses = TRUE)


# Applying every ML methods and compare each other and choosing best fits:

nrows <- NROW(wbcd)
set.seed(218)                           ## fix random value
index <- sample(1:nrows, 0.7 * nrows)   ## shuffle and divide

#train <- wbcd                          ## 569 test data (100%)
train <- wbcd[index,]                   ## 398 test data (70%)
test <- wbcd[-index,]                   ## 171 test data (30%)

prop.table(table(train$diagnosis))

prop.table(table(test$diagnosis))


# Applying different ML methods to the data:

library(caret)
#c5.0
install.packages("C50")
library(C50)
learn_c50 <- C5.0(train[,-1],train$diagnosis)
pre_c50 <- predict(learn_c50, test[,-1])
cm_c50 <- confusionMatrix(pre_c50, test$diagnosis)
cm_c50

#c5.0-tune
library(C50)

acc_test <- numeric()
accuracy1 <- NULL; accuracy2 <- NULL

for(i in 1:50){
  learn_imp_c50 <- C5.0(train[,-1],train$diagnosis,trials = i)      
  p_c50 <- predict(learn_imp_c50, test[,-1]) 
  accuracy1 <- confusionMatrix(p_c50, test$diagnosis)
  accuracy2[i] <- accuracy1$overall[1]
}

acc <- data.frame(t= seq(1,50), cnt = accuracy2)

opt_t <- subset(acc, cnt==max(cnt))[1,]
sub <- paste("Optimal number of trials is", opt_t$t, "(accuracy :", opt_t$cnt,") in C5.0")

install.packages("highcharter")
library(highcharter)
hchart(acc, 'line', hcaes(t, cnt)) %>%
  hc_title(text = "Accuracy With Varying Trials (C5.0)") %>%
  hc_subtitle(text = sub) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_xAxis(title = list(text = "Number of Trials")) %>%
  hc_yAxis(title = list(text = "Accuracy"))

#rpart:
library(rpart)
learn_rp <- rpart(diagnosis~.,data=train,control=rpart.control(minsplit=2))
pre_rp <- predict(learn_rp, test[,-1], type="class")
cm_rp  <- confusionMatrix(pre_rp, test$diagnosis)   
cm_rp

#prune:
learn_pru <- prune(learn_rp, cp=learn_rp$cptable[which.min(learn_rp$cptable[,"xerror"]),"CP"])
pre_pru <- predict(learn_pru, test[,-1], type="class")
cm_pru <-confusionMatrix(pre_pru, test$diagnosis)           
cm_pru

#oneR:
install.packages("RWeka")
library("RWeka")
learn_1r <- OneR(diagnosis~., data=train)
pre_1r <- predict(learn_1r, test[,-1])
cm_1r   <- confusionMatrix(pre_1r, test$diagnosis)
cm_1r

#JRip:
learn_jrip <- JRip(diagnosis ~ ., data=train)
pre_jrip <- predict(learn_jrip, test[,-1])
cm_jrip <- confusionMatrix(pre_jrip, test$diagnosis)        
cm_jrip

#naiveBayes:
install.packages("e1071")
library(e1071)

acc_test <- numeric()
accuracy1 <- NULL; accuracy2 <- NULL

for(i in 1:30){
  learn_imp_nb <- naiveBayes(train[,-1], train$diagnosis, laplace=i)    
  p_nb <- predict(learn_imp_nb, test[,-1]) 
  accuracy1 <- confusionMatrix(p_nb, test$diagnosis)
  accuracy2[i] <- accuracy1$overall[1]
}

acc <- data.frame(l= seq(1,30), cnt = accuracy2)

opt_l <- subset(acc, cnt==max(cnt))[1,]
sub <- paste("Optimal number of laplace is", opt_l$l, "(accuracy :", opt_l$cnt,") in naiveBayes")

library(highcharter)
hchart(acc, 'line', hcaes(l, cnt)) %>%
  hc_title(text = "Accuracy With Varying Laplace (naiveBayes)") %>%
  hc_subtitle(text = sub) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_xAxis(title = list(text = "Number of Laplace")) %>%
  hc_yAxis(title = list(text = "Accuracy"))

#randomForest:
install.packages("randomForest")
library(randomForest)
learn_rf <- randomForest(diagnosis~., data=train, ntree=500, proximity=T, importance=T)
pre_rf   <- predict(learn_rf, test[,-1])
cm_rf    <- confusionMatrix(pre_rf, test$diagnosis)
cm_rf

#ctree:
install.packages("party")
library(party)
learn_ct <- ctree(diagnosis~., data=train, controls=ctree_control(maxdepth=2))
pre_ct   <- predict(learn_ct, test[,-1])
cm_ct    <- confusionMatrix(pre_ct, test$diagnosis)
cm_ct

#KNN-tune:
library(class)

acc_test <- numeric() 

for(i in 1:30){
  predict <- knn(train=train[,-1], test=test[,-1], cl=train[,1], k=i, prob=T)
  acc_test <- c(acc_test,mean(predict==test[,1]))
}

acc <- data.frame(k= seq(1,30), cnt = acc_test)

opt_k <- subset(acc, cnt==max(cnt))[1,]
sub <- paste("Optimal number of k is", opt_k$k, "(accuracy :", opt_k$cnt,") in KNN")

library(highcharter)
hchart(acc, 'line', hcaes(k, cnt)) %>%
  hc_title(text = "Accuracy With Varying K (KNN)") %>%
  hc_subtitle(text = sub) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_xAxis(title = list(text = "Number of Neighbors(k)")) %>%
  hc_yAxis(title = list(text = "Accuracy"))


#KMeans:
predict.kmeans <- function(newdata, object){
  centers <- object$centers
  n_centers <- nrow(centers)
  dist_mat <- as.matrix(dist(rbind(centers, newdata)))
  dist_mat <- dist_mat[-seq(n_centers), seq(n_centers)]
  max.col(-dist_mat)
}

#GBM:
install.packages("gbm")
library(gbm)
test_gbm <- gbm(diagnosis~., data=train, distribution="gaussian",n.trees = 10000,
                shrinkage = 0.01, interaction.depth = 4, bag.fraction=0.5, train.fraction=0.5,n.minobsinnode=10,cv.folds=3,keep.data=TRUE,verbose=FALSE,n.cores=1)
best.iter <- gbm.perf(test_gbm, method="cv",plot.it=FALSE)
fitControl = trainControl(method="cv", number=5, returnResamp="all")
learn_gbm = train(diagnosis~., data=train, method="gbm", distribution="bernoulli", trControl=fitControl, verbose=F, tuneGrid=data.frame(.n.trees=best.iter, .shrinkage=0.01, .interaction.depth=1, .n.minobsinnode=1))
pre_gbm <- predict(learn_gbm, test[,-1])
cm_gbm <- confusionMatrix(pre_gbm, test$diagnosis)
cm_gbm


#Ada boost:
install.packages("rpart")
install.packages("ada")
library(rpart)
library(ada)
control <- rpart.control(cp = -1, maxdepth = 14,maxcompete = 1,xval = 0)
learn_ada <- ada(diagnosis~., data = train, test.x = train[,-1], test.y = train[,1], type = "gentle", control = control, iter = 70)
pre_ada <- predict(learn_ada, test[,-1])
cm_ada <- confusionMatrix(pre_ada, test$diagnosis)
cm_ada

#SVM:
learn_svm <- svm(diagnosis~., data=train)
pre_svm <- predict(learn_svm, test[,-1])
cm_svm <- confusionMatrix(pre_svm, test$diagnosis)
cm_svm


#SVM-tune:
gamma <- seq(0,0.1,0.005)
cost <- 2^(0:5)
parms <- expand.grid(cost=cost, gamma=gamma)    ## 231

acc_test <- numeric()
accuracy1 <- NULL; accuracy2 <- NULL

for(i in 1:NROW(parms)){        
  learn_svm <- svm(diagnosis~., data=train, gamma=parms$gamma[i], cost=parms$cost[i])
  pre_svm <- predict(learn_svm, test[,-1])
  accuracy1 <- confusionMatrix(pre_svm, test$diagnosis)
  accuracy2[i] <- accuracy1$overall[1]
}

acc <- data.frame(p= seq(1,NROW(parms)), cnt = accuracy2)

opt_p <- subset(acc, cnt==max(cnt))[1,]
sub <- paste("Optimal number of parameter is", opt_p$p, "(accuracy :", opt_p$cnt,") in SVM")

library(highcharter)
hchart(acc, 'line', hcaes(p, cnt)) %>%
  hc_title(text = "Accuracy With Varying Parameters (SVM)") %>%
  hc_subtitle(text = sub) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_xAxis(title = list(text = "Number of Parameters")) %>%
  hc_yAxis(title = list(text = "Accuracy"))

#Apply optimal parameters(gamma, cost) to show best predict performance in SVM:
learn_imp_svm <- svm(diagnosis~., data=train, cost=parms$cost[opt_p$p], gamma=parms$gamma[opt_p$p])
pre_imp_svm <- predict(learn_imp_svm, test[,-1])
cm_imp_svm <- confusionMatrix(pre_imp_svm, test$diagnosis)
cm_imp_svm

#Visualize to compare the accuracy of all methods:

col <- c("#ed3b3b", "#0099ff")
par(mfrow=c(3,5))
fourfoldplot(cm_c50$table, color = col, conf.level = 0, margin = 1, main=paste("C5.0 (",round(cm_c50$overall[1]*100),"%)",sep=""))
fourfoldplot(cm_imp_c50$table, color = col, conf.level = 0, margin = 1, main=paste("Tune C5.0 (",round(cm_imp_c50$overall[1]*100),"%)",sep=""))
fourfoldplot(cm_rp$table, color = col, conf.level = 0, margin = 1, main=paste("RPart (",round(cm_rp$overall[1]*100),"%)",sep=""))
fourfoldplot(cm_pru$table, color = col, conf.level = 0, margin = 1, main=paste("Prune (",round(cm_pru$overall[1]*100),"%)",sep=""))
fourfoldplot(cm_1r$table, color = col, conf.level = 0, margin = 1, main=paste("OneR (",round(cm_1r$overall[1]*100),"%)",sep=""))
fourfoldplot(cm_jrip$table, color = col, conf.level = 0, margin = 1, main=paste("JRip (",round(cm_jrip$overall[1]*100),"%)",sep=""))
fourfoldplot(cm_ct$table, color = col, conf.level = 0, margin = 1, main=paste("CTree (",round(cm_ct$overall[1]*100),"%)",sep=""))
fourfoldplot(cm_nb$table, color = col, conf.level = 0, margin = 1, main=paste("NaiveBayes (",round(cm_nb$overall[1]*100),"%)",sep=""))
fourfoldplot(cm_knn$table, color = col, conf.level = 0, margin = 1, main=paste("Tune KNN (",round(cm_knn$overall[1]*100),"%)",sep=""))
fourfoldplot(cm_kmeans$table, color = col, conf.level = 0, margin = 1, main=paste("KMeans (",round(cm_kmeans$overall[1]*100),"%)",sep=""))
fourfoldplot(cm_rf$table, color = col, conf.level = 0, margin = 1, main=paste("RandomForest (",round(cm_rf$overall[1]*100),"%)",sep=""))
fourfoldplot(cm_gbm$table, color = col, conf.level = 0, margin = 1, main=paste("GBM (",round(cm_gbm$overall[1]*100),"%)",sep=""))
fourfoldplot(cm_ada$table, color = col, conf.level = 0, margin = 1, main=paste("AdaBoost (",round(cm_ada$overall[1]*100),"%)",sep=""))
fourfoldplot(cm_svm$table, color = col, conf.level = 0, margin = 1, main=paste("SVM (",round(cm_svm$overall[1]*100),"%)",sep=""))
fourfoldplot(cm_imp_svm$table, color = col, conf.level = 0, margin = 1, main=paste("Tune SVM (",round(cm_imp_svm$overall[1]*100),"%)",sep=""))


#Select a best prediction model according to high accuracy:

opt_predict <- c(cm_c50$overall[1], cm_imp_c50$overall[1], cm_rp$overall[1], cm_pru$overall[1], cm_1r$overall[1], cm_jrip$overall[1], cm_ct$overall[1], cm_nb$overall[1], cm_knn$overall[1], cm_kmeans$overall[1], cm_rf$overall[1], cm_gbm$overall[1], cm_ada$overall[1], cm_svm$overall[1], cm_imp_svm$overall[1])
names(opt_predict) <- c("c50","imp_c50","rpart","prune","1r","jrip","ctree","nb","knn","kmeans","rf","gbm","ada","svm","imp_svm")
best_predict_model <- subset(opt_predict, opt_predict==max(opt_predict))
best_predict_model

#Prepare Patient data for testing function:
patient <- read.csv("Users/lana-n/datacsv/data_breast_wisconsin.csv", header=T, stringsAsFactors=F)
patient$X <- NULL
M <- patient[19,]               ## 19th patient
M[,c(1,2)]                      ## Malignant
B <- patient[20,]               ## 20th patient          
B[,c(1,2)]  

#Delete diagnosis column for testing:
M$diagnosis <- NULL
B$diagnosis <- NULL



#Patient Cancer Diagnosis Prediction Function
  
# for print output
cancer_diagnosis_predict_p <- function(new, method=learn_imp_svm) {
  new_pre <- predict(method, new[,-1])
  new_res <- as.character(new_pre)
  return(paste("Patient ID: ",new[,1],"  =>  Result: ", new_res, sep=""))
}

# for submission output
cancer_diagnosis_predict_s <- function(new, method=learn_imp_svm) {
  new_pre <- predict(method, new[,-1])
  new_res <- as.character(new_pre)
  return(new_res)
}

#Testing Function (use only 1 test data):
cancer_diagnosis_predict_p(B) 

cancer_diagnosis_predict_p(M)


cancer_diagnosis_predict_p(M,learn_imp_c50) 

#Make Submission Output (use test dataset)


t <- patient[-index,]
orgin <- t$diagnosis
t$diagnosis <- NULL
r <- cancer_diagnosis_predict_s(t)

sub <- data.frame(id=t$id, predict_diagnosis=ifelse(r=='Malignant','M','B'), orgin_diagnosis=orgin)
sub$correct <- ifelse(sub$predict_diagnosis == sub$orgin_diagnosis, "True", "False")
kable(head(sub,10))

#write.csv(sub[,c(1,2)], file='submission.csv', row.names=F)


# Visualize (Probabilty Density Function Graph):
#Create Visualize Function:
cancer_summary <- function(new,data) {
  
  ## [a] Reshape the new dataset for ggplot
  library(reshape2)
  m_train <- melt(data, id="diagnosis")
  m_new <- melt(new[,-1])
  
  
  ## [b] Variable To Highlight the key factors (geom_vline-RED)
  key_factors <- c("radius_mean","perimeter_mean","area_mean","perimeter_worst",
                   "texture_worst","radius_worst","symmetry_se","compactness_worst",
                   "concavity_worst","dimension_worst")
  
  key_col <- ifelse(m_new$variable %in% key_factors,"red","black")
  
  
  ## [c] Save mean of Malignant value & colors
  library(dplyr)
  mal_mean <- subset(data, diagnosis=="Malignant", select=-1)
  mal_mean <- apply(mal_mean,2,mean)
  
  library(stringr)
  mal_col <- ifelse((round(m_new$value,3) > mal_mean) & (str_count(m_new$variable, 'worst') < 1), "red", "black")
  
  
  
  ## [d] Save titles : Main title, Patient Diagnosis
  
  title <- "Breast Cancer Diagnosis Plot"
  subtitle <- cancer_diagnosis_predict_p(new)
  
  
  
  ## ★[e] View plot highlighting your manual key factor
  library(ggplot2)
  
  res_key <- ggplot(m_train, aes(x=value,color=diagnosis, fill=diagnosis))+
    geom_histogram(aes(y=..density..), alpha=0.5, position="identity", bins=50)+
    geom_density(alpha=.2)+
    scale_color_manual(values=c("#15c3c9","#f87b72"))+
    scale_fill_manual(values=c("#61d4d6","#f5a7a1"))+
    geom_vline(data=m_new, aes(xintercept=value), 
               color=key_col, size=1.5)+
    geom_label(data=m_new, aes(x=Inf, y=Inf, label=round(value,3)), nudge_y=2,  
               vjust = "top", hjust = "right", fill="white", color="black")+
    labs(title=paste(title,"(highlight Key Factors)"), subtitle=subtitle)+
    theme(plot.title = element_text(face='bold', colour='black', hjust=0.5, size=15))+
    theme(plot.subtitle=element_text(lineheight=0.8, hjust=0.5))+
    labs(caption="[Training 569 wisc cancer diagnostic patient data]")+
    facet_wrap(~variable, scales="free", ncol=5)
  
  
  
  ## ★[f] View plots highlighting values above average of malignant patient
  res_mean <- ggplot(m_train, aes(x=value,color=diagnosis, fill=diagnosis))+
    geom_histogram(aes(y=..density..), alpha=0.5, position="identity", bins=50)+
    geom_density(alpha=.2)+
    scale_color_manual(values=c("#15c3c9","#f87b72"))+
    scale_fill_manual(values=c("#61d4d6","#f5a7a1"))+
    geom_vline(data=m_new, aes(xintercept=value), 
               color=mal_col, size=1.5)+
    geom_label(data=m_new, aes(x=Inf, y=Inf, label=round(value,3)), nudge_y=2,  
               vjust = "top", hjust = "right", fill="white", color="black")+
    labs(title=paste(title,"(highlight Above malignant average)"), subtitle=subtitle)+
    theme(plot.title = element_text(face='bold', colour='black', hjust=0.5, size=15))+
    theme(plot.subtitle=element_text(lineheight=0.8, hjust=0.5, size=12))+
    labs(caption="[Training 569 wisc cancer diagnostic patient data]")+
    facet_wrap(~variable, scales="free", ncol=5)
  
  
  
  ## [g] output graph
  res_mean
  #res_key}


#Testing Function:


cancer_summary(B, wbcd)

cancer_summary(M, wbcd)

#Visualize (Radar):


cancer_radar <- function(new,data) {
  
  ## [a] Radar Function
  coord_radar <- function (theta = "x", start = 0, direction = 1) 
  {
    theta <- match.arg(theta, c("x", "y"))
    r <- ifelse(theta == "x", "y", "x")
    ggproto("CoordRadar", CoordPolar, theta = theta, r = r, start = start, 
            direction = sign(direction),
            is_linear = function(coord) TRUE)
  }
  
  
  ## [b] Normalize Function -> you can use rescale instead.
  normalize <- function(x) {
    return((x-min(x))/(max(x)-min(x)))
  }
  
  
  ## [c] Get average from Normal(Benign) Data to set standards (Grey area)
  b1 <- subset(data, diagnosis=="Benign", select=-1)
  b2 <- as.data.frame(lapply(b1,normalize))           
  be <- colMeans(b2)
  
  
  ## [d] Normalize Patient Data to compare with normal dataset
  p_new <- (new[,-1]-apply(b1,2,min))/(apply(b1,2,max)-apply(b1,2,min))
  max_value <- max(p_new)
  
  
  ## [e] Combine Two data (Normal, Patient)
  cc_radar <- rbind(be,p_new)
  cc_radar <- cbind(group=c("Normal","Patient"),cc_radar)
  
  coc <- melt(cc_radar, id="group")
  library(stringr)
  coc$variable <- as.character(coc$variable)
  coc$variable[str_count(coc$variable,'\\_')>1] <- sub('_', '.', coc$variable[str_count(coc$variable,'\\_')>1])
  name <- unlist(strsplit(as.character(coc$variable),"_"))
  
  coc$feature <- name[c(seq(1,length(name),2))]
  coc$type <- name[c(seq(2,length(name),2))]  
  coc$variable <- NULL
  
  df <- coc[order(coc$feature),]
  
  
  ## [f] Save titles : Main title, Patient Diagnosis
  title <- "Breast Cancer Diagnosis Radar"
  subtitle <- cancer_diagnosis_predict_p(new)
  
  
  
  ## ★[g] Radar plot
  res <- ggplot(df, aes(x=feature,y=value,group=group,fill=group,color=group))+
    geom_point()+geom_polygon(alpha=0.3)+coord_radar()+ylim(0,max_value)+
    scale_color_manual(values=c(NA,"#b10000"))+
    scale_fill_manual(values=c("#8e8e8e",NA))+
    facet_wrap(~type)+
    theme(panel.background=element_rect(fill = "white", colour= NA),
          panel.border=element_rect(fill = NA, colour="grey50"), 
          panel.grid.major=element_line(colour = "grey90", size = 0.2),
          panel.grid.minor=element_line(colour = "grey98", size = 0.5),
          legend.position="bottom",
          strip.background =  element_rect(fill = "grey80", colour = "grey50"),
          axis.text.y=element_text(colour=NA),
          axis.title.y=element_text(colour=NA),
          axis.ticks=element_line(colour = NA))+
    xlab("")+ylab("")+
    labs(title=title, subtitle=subtitle)+
    theme(plot.title = element_text(face='bold', colour='black', hjust=0.5, size=15))+
    theme(plot.subtitle=element_text(lineheight=0.8, hjust=0.5, size=12))+
    labs(caption="[Training 569 wisc cancer diagnostic patient data]")
  
  
  
  ## [h] output graph
  res
  
}


#Testing Function:

cancer_radar(B,wbcd)

cancer_radar(M,wbcd) 

























































































