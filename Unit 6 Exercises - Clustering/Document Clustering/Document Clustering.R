

## Clustering blog articles based on content 
## from website Daily Kos


##load packages and read data
scipen=999
setwd("Unit 6 Exercises - Clustering/Document Clustering")
pacman::p_load(tidyverse, caTools, ROCR, rpart, rpart.plot, randomForest, caret, e1071, rattle, tm, SnowballC)

dailykos = read.csv("dailykos.csv")


## hierarchical clustering
kosDist = dist(dailykos, method="euclidean")
kosHierClust = hclust(kosDist, method="ward.D")

plot(kosHierClust)
rect.hclust(kosHierClust ,k=3, border="red")

#segmenting into 7 clusters
hierGroups = cutree(kosHierClust, k = 7)
HierCluster1 = subset(dailykos, hierGroups == 1)
HierCluster2 = subset(dailykos, hierGroups == 2)
HierCluster3 = subset(dailykos, hierGroups == 3)
HierCluster4 = subset(dailykos, hierGroups == 4)
HierCluster5 = subset(dailykos, hierGroups == 5)
HierCluster6 = subset(dailykos, hierGroups == 6)
HierCluster7 = subset(dailykos, hierGroups == 7)

table(hierGroups)

#Most frequent words in cluster 1
tail(sort(colMeans(HierCluster1)))



## k-means clustering
set.seed(100)
KmeansCluster = kmeans(dailykos, centers=7)

KmeansCluster1 = subset(dailykos, KmeansCluster$cluster == 1)
KmeansCluster2 = subset(dailykos, KmeansCluster$cluster == 2)
KmeansCluster3 = subset(dailykos, KmeansCluster$cluster == 3)
KmeansCluster4 = subset(dailykos, KmeansCluster$cluster == 4)
KmeansCluster5 = subset(dailykos, KmeansCluster$cluster == 5)
KmeansCluster6 = subset(dailykos, KmeansCluster$cluster == 6)
KmeansCluster7 = subset(dailykos, KmeansCluster$cluster == 7)

table(KmeansCluster$cluster)

tail(sort(colMeans(KmeansCluster1)))


#comparing clusters
table(hierGroups, KmeansCluster$cluster)















