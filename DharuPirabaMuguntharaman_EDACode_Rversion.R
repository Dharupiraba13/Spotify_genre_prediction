music_data=read.csv('/Users/dharupirabamuguntharaman/Downloads/music_genre.csv',na.strings=c("NA", "NULL"))
head(music_data)
tail(music_data)
dim(music_data)
names(music_data)
length(names(music_data))
which(names(music_data)=='music_genre')
music_data=na.omit(music_data)
dim(music_data)
lbl=table(music_data$music_genre)
label_names=names(lbl)
label_count=as.numeric(lbl)
label_names
label_count
print(ifelse(length(lbl)==2,"Binary Classification","Multiclass Classification"))
Imbalance=function(data,labels,numUniqueLabels){
  rows=nrow(data)
  ideal_rows=rows*1/numUniqueLabels
  flag=0
  for (i in 1:numUniqueLabels){
    if(labels[[i]]<ideal_rows*1/(numUniqueLabels/2)|labels[[i]]>ideal_rows*(numUniqueLabels/2) ){
      print("Imbalanced class")
      print(names(labels)[i])
      flag=flag+1
    }
  }
  if (flag==0){
    print("No Imbalance class found in the dataset")
  }
}

Imbalance(music_data,lbl,10)
Find_null=function(data){
  if(is.null(data)){
    print("Dataset contains null values")
  }else{
    print("Dataset contains no null values")
  }
}
Find_null(music_data)
summary(music_data)
music_data=music_data[-c(1,2,3,16)]
dim(music_data)
summary(music_data)
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
attributes=names(music_data)
for (i in 1:13){
  print(ggplot(data=music_data,mapping=aes_string(x="music_genre",y=attributes[i]))+geom_boxplot())
}
music_data=music_data[music_data$tempo !='?', ] 
dim(music_data)
numeric_cols=select(music_data, is.numeric)
categorical_cols=select(music_data, is.character)
print(names(numeric_cols))
print(names(categorical_cols))
attributes=names(numeric_cols)
classes=sapply(numeric_cols,class)
i=1
for (col in numeric_cols[classes=='numeric']){
  title=paste("Histogram of",attributes[i],sep=" ")
  hist(col,xlab=attributes[i],main = title)
  i=i+1
}
music_data=select(music_data,-c(12))
names(music_data)
music_data$key <- as.factor(music_data$key)
music_data <- one_hot(as.data.table(music_data))
dim(music_data)
names(music_data)
music_data$mode <- as.factor(music_data$mode)
music_data <- one_hot(as.data.table(music_data))
dim(music_data)
names(music_data)
install.packages("ggcorrplot")
library(ggcorrplot)
cor_data=cor(numeric_cols)
ggcorrplot(cor_data,hc.order = TRUE,
           type = "lower",
           outline.color = "white")
duplicated(music_data)