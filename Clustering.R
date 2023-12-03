
#A. Loading the useful packages:
library(ggplot2) #for plotting
library(pROC) #for ROC analysis
library(mice) #for imputation
library(caret) #for clustering algorithms
library(plotly) #for plotting
library(tidyverse) #for data wrangling
library(factoextra) #for easy  extraction and visualization of inputs

#B. Data Pre-processing

#Importing the Dataset
y<-read.csv("E:/Modules/heart-c.csv")

#Take a look on the data

head(y)
tail(y)
summary(y)
str(y)

#Encoding Categorical Data into Numeric Data

#Sex Variable
for(i in 1:nrow(y)){
  if(y[i,3]=="male"){
    y[i,3]= 1 
  }else if(y[i,3]=="female"){
    y[i,3]= 0
  } 
}

#cp variable

for(r in 1:nrow(y)){
  if(y[r,4]=="asympt"){
    y[r,4]=4
  }else if(y[r,4]=="atyp_angina"){
    y[r,4]=2
  }else if(y[r,4]=="non_anginal"){
    y[r,4]=3
  }else if(y[r,4]=="typ_angina"){
    y[r,4]=1
  }
}

#y %>% group_by(fbs) %>% count()

#restecg variable

for(r in 1:nrow(y)){
  if(y[r,8]=="left_vent_hyper"){
    y[r,8]=2
  }else if(y[r,8]=="normal"){
    y[r,8]=0
  }else if(y[r,8]=="st_t_wave_abnormality"){
    y[r,8]=1
  }
}

#exang variable

for(r in 1:nrow(y)){
  if(y[r,10]=="no"){
    y[r,10]=0
  }else if(y[r,10]=="yes"){
    y[r,10]=1
  }
}

#fbs variable

for(r in 1:nrow(y)){
  if(y[r,7]=="f"){
    y[r,7]=0
  }else if(y[r,7]=="t"){
    y[r,7]=1
  }
}

#slope variable

for(r in 1:nrow(y)){
  if(y[r,12]=="down"){
    y[r,12]=3
  }else if(y[r,12]=="flat"){
    y[r,12]=2
  }else if(y[r,12]=="up"){
    y[r,12]=1
  }
}

#thal variable

y <- y %>% mutate(thal = ifelse(thal == "normal", 3, 
                                ifelse(thal=="fixed_defect",6,

                                                                              ifelse(thal=="reversable_defect",7,NA))))
#Quick check

str(y)

#Removing the independent variable

y<- y %>% select(-X)
y

#Re-checking

str(y)
summary(y)

Removing Missing Data

y<-na.omit(y)


#K-MEANS CLUSTERING
#Removing the “num” variable

x<-y[,-ncol(y)]

#Scaling the Data

scale_data<-scale(x)

#Determining the Euclidean Distance

distance<-dist(scale_data,method = "euclidean")
f <- c()
for(i in 1:10){
  f[i]<-kmeans(distance,i)$tot.withinss
  
}
plot(f,type = "l")

#Run the K-means algorithm and provide the optimum value of K.

km<-kmeans(distance,2)
km

#Clusters

km$cluster

x$group<-as.factor(km$cluster)

#Visualization

#Geom Density
x %>% gather("variable", "value", -group ) %>% ggplot(., mapping = aes(x = value, group = group, fill = group)) +
  geom_density(alpha = 0.5) + facet_wrap(~ variable, scales = "free")


#Geom Histogram
x %>% gather("variable", "value", -group ) %>% ggplot(., mapping = aes(x = value, group = group, fill = group)) +
  geom_histogram(alpha = 0.5) + facet_wrap(~ variable, scales = "free")

#Cluster plot visualization

fviz_cluster(list(data= x[,-ncol(x)], cluster = km$cluster ))


#2.HIERACHICHAL CLUSTERING

#Ward Linking Method

h<-hclust(distance, method = "ward") 
#Hierarchical clustering by Ward Linking method

plot(h) #Cluster Diagram


h2<-cutree(h, k=2)
h2

#Cluster Plot

fviz_cluster(list(data= x[,-ncol(x)], cluster = h2 ))












