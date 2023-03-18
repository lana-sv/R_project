#Project 07 - Adult Census Income Classification project:   Exploratory Analysis - Zillow

#Zillow’s Home Value prediction!

#https://www.kaggle.com/code/philippsp/exploratory-analysis-zillow
install.packages("data.table")
install.packages("ggplot2")
install.packages("stats")
install.packages("stringr")
install.packages("extrafont")
install.packages("knitr")
install.packages("lmtest")
install.packages("sandwich")
install.packages("zoo")
install.packages("leaflet")
library("data.table")
library("ggplot2")
library("stats")
library("stringr")
library("extrafont")
library("knitr")
library("lmtest")
library("sandwich")
library("zoo")
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
library(leaflet)
library(lubridate)

properties <- fread('/Users/lana-n/datacsv/properties_2016.csv')
properties17 <- fread('/Users/lana-n/datacsv/properties_2017.csv')
transactions <- fread('/Users/lana-n/datacsv/train_2016.csv')
transactions17 <- fread('/Users/lana-n/datacsv/train_2017.csv')
#sample_submission <- fread('../input/sample_submission.csv')

# Other Functions & Libraries

install.packages("randomForest")
install.packages("xgboost")
install.packages("class")

library("randomForest")  # For Random Forest Exploration.
library("xgboost")       # In order to use Gradient Boosting algorithm.
library("class")         # In order to use k nearest neighbours classification algorithm.
f1score = function(precision, recall){2 * precision * recall / (precision + recall)}


#Renaming the features:
properties <- properties %>% rename(
  id_parcel = parcelid,
  build_year = yearbuilt,
  area_basement = basementsqft,
  area_patio = yardbuildingsqft17,
  area_shed = yardbuildingsqft26, 
  area_pool = poolsizesum,  
  area_lot = lotsizesquarefeet, 
  area_garage = garagetotalsqft,
  area_firstfloor_finished = finishedfloor1squarefeet,
  area_total_calc = calculatedfinishedsquarefeet,
  area_base = finishedsquarefeet6,
  area_live_finished = finishedsquarefeet12,
  area_liveperi_finished = finishedsquarefeet13,
  area_total_finished = finishedsquarefeet15,  
  area_unknown = finishedsquarefeet50,
  num_unit = unitcnt, 
  num_story = numberofstories,  
  num_room = roomcnt,
  num_bathroom = bathroomcnt,
  num_bedroom = bedroomcnt,
  num_bathroom_calc = calculatedbathnbr,
  num_bath = fullbathcnt,  
  num_75_bath = threequarterbathnbr, 
  num_fireplace = fireplacecnt,
  num_pool = poolcnt,  
  num_garage = garagecarcnt,  
  region_county = regionidcounty,
  region_city = regionidcity,
  region_zip = regionidzip,
  region_neighbor = regionidneighborhood,  
  tax_total = taxvaluedollarcnt,
  tax_building = structuretaxvaluedollarcnt,
  tax_land = landtaxvaluedollarcnt,
  tax_property = taxamount,
  tax_year = assessmentyear,
  tax_delinquency = taxdelinquencyflag,
  tax_delinquency_year = taxdelinquencyyear,
  zoning_property = propertyzoningdesc,
  zoning_landuse = propertylandusetypeid,
  zoning_landuse_county = propertycountylandusecode,
  flag_fireplace = fireplaceflag, 
  flag_tub = hashottuborspa,
  quality = buildingqualitytypeid,
  framing = buildingclasstypeid,
  material = typeconstructiontypeid,
  deck = decktypeid,
  story = storytypeid,
  heating = heatingorsystemtypeid,
  aircon = airconditioningtypeid,
  architectural_style= architecturalstyletypeid
)
transactions <- transactions %>% rename(
  id_parcel = parcelid,
  date = transactiondate
)

properties <- properties %>% 
  mutate(tax_delinquency = ifelse(tax_delinquency=="Y",1,0),
         flag_fireplace = ifelse(flag_fireplace=="Y",1,0),
         flag_tub = ifelse(flag_tub=="Y",1,0))

#Distribution of transaction dates:

tmp <- transactions %>% mutate(year_month = make_date(year=year(date),month=month(date)))
tmp %>% 
  group_by(year_month) %>% count() %>% 
  ggplot(aes(x=year_month,y=n)) +
  geom_bar(stat="identity", fill="red")+
  geom_vline(aes(xintercept=as.numeric(as.Date("2016-10-01"))),size=2)

#outcome:

transactions %>% 
  ggplot(aes(x=logerror)) + 
  geom_histogram(bins=400, fill="red")+
  theme_bw()+theme(axis.title = element_text(size=16),axis.text = element_text(size=14))+
  ylab("Count")+coord_cartesian(x=c(-0.5,0.5))


transactions <- transactions %>% mutate(abs_logerror = abs(logerror))
transactions %>% 
  ggplot(aes(x=abs_logerror)) + 
  geom_histogram(bins=400, fill="red")+
  theme_bw()+theme(axis.title = element_text(size=16),axis.text = element_text(size=14))+
  ylab("Count")+coord_cartesian(x=c(0,0.5))


transactions %>% 
  mutate(year_month = make_date(year=year(date),month=month(date)) ) %>% 
  group_by(year_month) %>% summarize(mean_abs_logerror = mean(abs_logerror)) %>% 
  ggplot(aes(x=year_month,y=mean_abs_logerror)) + 
  geom_line(size=1.5, color="red")+
  geom_point(size=5, color="red")+theme_bw()

transactions %>% 
  mutate(year_month = make_date(year=year(date),month=month(date)) ) %>% 
  group_by(year_month) %>% summarize(mean_logerror = mean(logerror)) %>% 
  ggplot(aes(x=year_month,y=mean_logerror)) + 
  geom_line(size=1.5, color="red")+geom_point(size=5, color="red")+theme_bw()


missing_values <- properties %>% summarize_each(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values, key="feature", value="missing_pct")
missing_values %>% 
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="red")+
  coord_flip()+theme_bw()

good_features <- filter(missing_values, missing_pct<0.75)

vars <- good_features$feature[str_detect(good_features$feature,'num_')]

cor_tmp <- transactions %>% left_join(properties, by="id_parcel") 
tmp <- cor_tmp %>% select(one_of(c(vars,"abs_logerror")))

corrplot(cor(tmp, use="complete.obs"),type="lower")




vars <- good_features$feature[str_detect(good_features$feature,'area_')]

tmp <- cor_tmp %>% select(one_of(c(vars,"abs_logerror")))

corrplot(cor(tmp, use="complete.obs"), type="lower")


vars <- setdiff(good_features$feature[str_detect(good_features$feature,'tax_')],c("tax_delinquency","tax_year"))

tmp <- cor_tmp %>% select(one_of(c(vars,"abs_logerror")))

corrplot(cor(tmp, use="complete.obs"), type="lower")



vars <- good_features$feature[str_detect(good_features$feature,'num_')]

cor_tmp <- transactions %>% left_join(properties, by="id_parcel") 
tmp <- cor_tmp %>% select(one_of(c(vars,"logerror")))

corrplot(cor(tmp, use="complete.obs"),type="lower")



vars <- good_features$feature[str_detect(good_features$feature,'area_')]

tmp <- cor_tmp %>%  select(one_of(c(vars,"logerror")))

corrplot(cor(tmp, use="complete.obs"), type="lower")




vars <- setdiff(good_features$feature[str_detect(good_features$feature,'tax_')],c("tax_delinquency","tax_year"))

tmp <- cor_tmp %>%  select(one_of(c(vars,"logerror")))

corrplot(cor(tmp, use="complete.obs"), type="lower")

#When were the houses built?
cor_tmp %>% 
  ggplot(aes(x=build_year))+geom_line(stat="density", color="red", size=1.2)+theme_bw()


cor_tmp %>% 
  group_by(build_year) %>% 
  summarize(mean_abs_logerror = mean(abs(logerror)),n()) %>% 
  ggplot(aes(x=build_year,y=mean_abs_logerror))+
  geom_smooth(color="grey40")+
  geom_point(color="red")+coord_cartesian(ylim=c(0,0.25))+theme_bw()


cor_tmp %>% 
  group_by(build_year) %>% 
  summarize(mean_logerror = mean(logerror)) %>% 
  ggplot(aes(x=build_year,y=mean_logerror))+
  geom_smooth(color="grey40")+
  geom_point(color="red")+coord_cartesian(ylim=c(0,0.075))+theme_bw()


## `geom_smooth()` using method = 'loess' and formula 'y ~ x'


#Where does Zestimate predict well?

transactions <- transactions %>% mutate(percentile = cut(abs_logerror,quantile(abs_logerror, probs=c(0, 0.1, 0.25, 0.75, 0.9, 1),names = FALSE),include.lowest = TRUE,labels=FALSE))

tmp1 <- transactions %>% 
  filter(percentile == 1) %>% 
  sample_n(5000) %>% 
  left_join(properties, by="id_parcel")
tmp2 <- transactions %>% 
  filter(percentile == 5) %>% 
  sample_n(5000) %>% 
  left_join(properties, by="id_parcel")
tmp3 <- transactions %>% 
  filter(percentile == 3) %>% 
  sample_n(5000) %>% 
  left_join(properties, by="id_parcel")

tmp1 <- tmp1 %>% mutate(type="best_fit")
tmp2 <- tmp2 %>% mutate(type="worst_fit")
tmp3 <- tmp3 %>% mutate(type="typical_fit")


tmp <- bind_rows(tmp1,tmp2,tmp3)
tmp <- tmp %>% mutate(type = factor(type,levels = c("worst_fit", "typical_fit", "best_fit")))


col_pal <- "Set1"

tmp %>% ggplot(aes(x=latitude, fill=type, color=type)) + geom_line(stat="density", size=1.2) + theme_bw()+scale_fill_brewer(palette=col_pal)+scale_color_brewer(palette=col_pal)





tmptrans <- transactions %>% 
  left_join(properties, by="id_parcel")

tmptrans %>% 
  ggplot(aes(x=latitude,y=abs_logerror))+geom_smooth(color="red")+theme_bw()

#Where does Zestimate over or underpredict?
tmptrans <- tmptrans %>% mutate(overunder = ifelse(logerror<0,"under","over"))


leaflet() %>% 
  addTiles() %>% 
  fitBounds(-118.5,33.8,-118.25,34.15) %>% 
  addRectangles(-118.5,33.8,-118.25,34.15) %>% 
  addMiniMap()


#Where are all those properties?

lat <- range(properties$latitude/1e06,na.rm=T)
lon <- range(properties$longitude/1e06,na.rm=T)

tmp <- properties %>% 
  sample_n(2000) %>% 
  select(id_parcel,longitude,latitude) %>% 
  mutate(lon=longitude/1e6,lat=latitude/1e6) %>% 
  select(id_parcel,lat,lon) %>% 
  left_join(transactions,by="id_parcel")

leaflet(tmp) %>% 
  addTiles() %>% 
  fitBounds(lon[1],lat[1],lon[2],lat[2]) %>% 
  addCircleMarkers(stroke=FALSE) %>% 
  addMiniMap()

#Map absolute logerror
tmp <- transactions %>% 
  sample_n(2000) %>% 
  left_join(properties,by="id_parcel") %>% 
  select(id_parcel,longitude,latitude, abs_logerror) %>% 
  mutate(lon=longitude/1e6,lat=latitude/1e6) %>% 
  select(id_parcel,lat,lon, abs_logerror)

qpal <- colorQuantile("YlOrRd", tmp$abs_logerror, n = 7)

leaflet(tmp) %>% 
  addTiles() %>% 
  fitBounds(lon[1],lat[1],lon[2],lat[2]) %>% 
  addCircleMarkers(stroke=FALSE, color=~qpal(abs_logerror),fillOpacity = 1) %>% 
  addLegend("bottomright", pal = qpal, values = ~abs_logerror,title = "Absolute logerror",opacity = 1) %>% 
  addMiniMap()


#What do these features mean?















































