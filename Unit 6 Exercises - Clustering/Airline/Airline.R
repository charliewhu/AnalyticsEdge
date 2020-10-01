
## Clustering groups of customers into an Airline frequent flyer program

##load packages and read data
scipen=999
setwd("Unit 6 Exercises - Clustering/Airline")
pacman::p_load(tidyverse, caTools, ROCR, rpart, rpart.plot, randomForest, caret, e1071, rattle, tm, SnowballC, caret)

airlines = read.csv("AirlinesCluster.csv")
summary(airlines)


## Preprocess data
preproc = caret::preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm) #all variables now have mean==0


##Hierarchical Clustering
distances = dist(airlinesNorm, method="euclidean")
hierClust = hclust(distances, method="ward.D")
plot(hierClust)

clusterGroups = cutree(hierClust, k = 5) #create 5 cluster groups
table(clusterGroups) 

lapply(split(airlinesNorm, clusterGroups), colMeans) #compare means across clusters


##K-means Clustering
set.seed(88)
kmeansClust = kmeans(airlinesNorm, centers=5, iter.max=1000)
table(kmeansClust$cluster)





















