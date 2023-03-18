#Project 03 - Pokemon dataset:

# https://www.kaggle.com/code/jonathanbouchet/pokemon-data-clusters/data
# dataset downloaded from the link above
install.packages("fmsb")
install.packages("corrplot")
install.packages("corrgram")
install.packages("fmsb")
install.packages("gplots")
install.packages("RColorBrewer")
library(ggplot2)
library(dplyr)
library(gridExtra)
library(fmsb)
library(corrplot)
library(corrgram)
library(caTools)
library(gplots)
library(RColorBrewer)


df<-read.csv('pokemon_alopez247.csv',sep=',')

#I group by the Pokemons by their `Body_type` and take the average of each numeric columns

group<-df %>% filter(hasGender=='True') %>% 
  group_by(Body_Style) %>% 
  select(HP,Attack,Defense,Sp_Atk,Sp_Def,Speed,Pr_Male,Height_m, Weight_kg, Catch_Rate) %>% 
  summarise(avgHP = mean(Attack),avgDefense = mean(Defense),avgSPAttack = mean(Sp_Atk),avgSPDef = mean(Sp_Def),
            avgProbMale = mean(Pr_Male),avgHeight = mean(Height_m),avgWeight = mean(Weight_kg),avgCatch = mean(Catch_Rate))


#I define the min/max range of each column (needed for the Radar plots)
max<-c(100,100,100,100,1,3,100,200)
min<-rep(0,8)

par(mfrow=c(4,4))
par(mar=c(1,1,1,1))
for(i in 1:nrow(group)){
  radarchart(  rbind(max,min,group[i,2:9]), axistype=2 , pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , 
               plwd=4 , cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,2000,5), cglwd=0.8, vlcex=0.6 ,title=as.character(group$Body_Style[i]) )
}

groupDF<-as.data.frame(group)
row.names(groupDF) <- groupDF$Body_Style
groupDF <- groupDF[,2:9]
group_matrix <- data.matrix(groupDF)
heatmap.2(group_matrix, Rowv=FALSE, Colv=FALSE, dendrogram='none', cellnote=round(group_matrix,digits=2), notecol="black", trace='none', key=FALSE,lwid = c(.01,.99),lhei = c(.01,.99),margins = c(8,16))

#select the average of Total (sum of all characteristics) and reorder the result
group1<-df %>% filter(hasGender=='True') %>% group_by(Body_Style) %>% select(Total) %>% summarise(avgTotal = mean(Total))
group1 %>% arrange(desc(avgTotal))

group1$ReorderedBody <- reorder(group1$Body_Style, group1$avgTotal)

#define the colorPallette
colourCount = length(unique(group$Body_Style))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

#define the label for each bar
group1$LABEL <-paste0(round(group1$avgTotal))
ggplot(group1, aes(x=ReorderedBody, y=avgTotal, fill=factor(ReorderedBody))) + geom_bar(width = 0.9, stat="identity") + scale_fill_manual(values = getPalette(colourCount)) +coord_polar(theta = "y") + xlab("") + ylab("") + ylim(c(0,max(group1$avgTotal)))  + ggtitle("Average Total per Body Style") + geom_text(data = group1, hjust = 1, size = 3, aes(x = Body_Style, y = 0, label = LABEL)) + theme_minimal() + guides(fill=guide_legend(title=NULL,reverse=TRUE)) + theme(
  panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_blank(),axis.text.y = element_blank(),axis.text.x = element_blank(),axis.ticks = element_blank(),plot.title = element_text(hjust = 0.5))


groupMedian<-df %>% filter(hasGender=='True') %>% group_by(Body_Style) %>% select(HP,Attack,Defense,Sp_Atk,Sp_Def,Speed,Pr_Male,Height_m, Weight_kg, Catch_Rate) %>% summarise(medHP = median(Attack),medDefense = median(Defense),medSPAttack = median(Sp_Atk),medSPDef = median(Sp_Def),medProbMale = median(Pr_Male),medHeight = median(Height_m),medWeight = median(Weight_kg),medCatch = median(Catch_Rate))

byMean<-rbind(max,min,group[1,2:9])
byMedian<-rbind(max,min,groupMedian[1,2:9])
op <- par(mar=c(1, 2, 2, 1),mfrow=c(1, 2))
radarchart( byMean ,vlcex=.6, title = "Bipedal_tailed : mean")
radarchart( byMedian ,vlcex=.6,title = "Bipedal_tailed : median")

g1<-ggplot(data=filter(df,hasGender=='True' & Body_Style=='bipedal_tailed'),aes(x=Pr_Male)) + geom_histogram(bins=100)
g2<-ggplot(data=filter(df,hasGender=='True' & Body_Style=='bipedal_tailed'),aes(x=Catch_Rate)) + geom_histogram(bins=100)
g3<-ggplot(data=filter(df,hasGender=='True' & Body_Style=='bipedal_tailed'),aes(x=Attack)) + geom_histogram(bins=100)
grid.arrange(g1,g2,g3,ncol=3)


df2<-df %>% filter(hasGender=='True' & Body_Style=='bipedal_tailed') %>% select(-Number)
num.cols <- sapply(df2, is.numeric)
cor.data <- cor(df2[,num.cols])
corrPLOT<-corrplot(cor.data,method='ellipse')


df3<-df %>% filter(hasGender=='True') %>% select(-Generation)
#split data into training/testing
set.seed(101)
split<-sample.split(df3$Number,SplitRatio=.7)
train<-subset(df3,split==T)
test<-subset(df3,split==F)

#define a function to summarize a given model
plotRes<-function(mod){
  print(mod)
  summary(mod)
  #create DF with prediction and real values
  mod.predictions <- predict(mod,test)
  mod.res<- cbind(mod.predictions,test$Catch_Rate)
  colnames(mod.res) <- c('pred','real')
  mod.res <- as.data.frame(mod.res)
  #make plots of residuals,etc...
  g1<-ggplot(data=mod.res,aes(x=pred,y=real)) + geom_point() + geom_abline(intercept = 0, slope = 1, color="red")
  g2<-ggplot(data=mod.res,aes(x=real-pred)) + geom_histogram(bins=100)
  g3<-ggplot(data=mod.res,aes(x=pred,y=real-pred)) + geom_point()
  grid.arrange(g1,g2,g3,nrow=2, ncol=2)
  #calculate metrics
  mse <- mean((mod.res$real-mod.res$pred)^2)
  rmse<-mse^0.5
  SSE = sum((mod.res$pred - mod.res$real)^2)
  SST = sum( (mean(test$Catch_Rate) - mod.res$real)^2)
  R2 = 1 - SSE/SST
  sprintf("MSE: %f RMSE : %f R2 :%f", mse,rmse,R2)}
  
  
#linear model
linModel<-lm(Catch_Rate ~  HP + Attack + Defense + Sp_Atk + Sp_Def + Speed + Pr_Male + Height_m + Weight_kg, train)
plotRes(linModel)















  













