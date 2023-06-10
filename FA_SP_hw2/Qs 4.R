# Load package required for plotting
library(ggplot2)
library(gridExtra)

#Easy to reproduce results in model
set.seed(1)
setwd("D:/OMS/Intro to Analytical Modelling/FA_SP_hw2/data 4/")

# Reading the data
iris_data <- read.table("iris.txt", header = TRUE, sep = "", dec = ".")
head(iris_data)

#Scale Data before applying KMeans
iris_data_scaled = iris_data 

# 5th column is Species name
for (i in 1:4) 
{ 
  iris_data_scaled[,i] = (iris_data[,i]-min(iris_data[,i]))/(max(iris_data[,i])-min(iris_data[,i])) 
}

# Kmeans clustering
cluster_1 = kmeans(iris_data_scaled[,1:4], 2, nstart = 10)
cluster_2 = kmeans(iris_data_scaled[,1:4], 3, nstart = 10)
cluster_3 = kmeans(iris_data_scaled[,1:4], 4, nstart = 10)
cluster_4 = kmeans(iris_data_scaled[,1:4], 5, nstart = 10)

# total distance between data points and cluster centers for different set of clusters:
cluster_dist <- rep(0,4)

#Measure sum of distances between each point and its respective cluster center
for (i in 1:nrow(iris_data)) {
  cluster_dist[1] = cluster_dist[1] + dist(rbind(iris_data[i,1:4],cluster_1$centers[cluster_1$cluster[i],]))
  cluster_dist[2] = cluster_dist[2] + dist(rbind(iris_data[i,1:4],cluster_2$centers[cluster_1$cluster[i],]))
  cluster_dist[3] = cluster_dist[3] + dist(rbind(iris_data[i,1:4],cluster_3$centers[cluster_1$cluster[i],]))
  cluster_dist[4] = cluster_dist[4] + dist(rbind(iris_data[i,1:4],cluster_4$centers[cluster_1$cluster[i],]))
}

cluster_dist
which.min(cluster_dist)

table(cluster_1$cluster, iris_data$Species)
table(cluster_2$cluster, iris_data$Species)
table(cluster_3$cluster, iris_data$Species)
table(cluster_4$cluster, iris_data$Species)

#Using just Sepal Length and Width
cluster_1_Sepal = kmeans(iris_data_scaled[,1:2], 2, nstart = 10)
cluster_2_Sepal = kmeans(iris_data_scaled[,1:2], 3, nstart = 10)
cluster_3_Sepal = kmeans(iris_data_scaled[,1:2], 4, nstart = 10)
cluster_4_Sepal = kmeans(iris_data_scaled[,1:2], 5, nstart = 10)

# total distance between data points and cluster centers for different set of clusters:
cluster_dist_Sepal <- rep(0,4)

#Measure sum of distances between each point and its respective cluster center
for (i in 1:nrow(iris_data)) {
  cluster_dist_Sepal[1] = cluster_dist_Sepal[1] + dist(rbind(iris_data[i,1:4],cluster_1_Sepal$centers[cluster_1_Sepal$cluster[i],]))
  cluster_dist_Sepal[2] = cluster_dist_Sepal[2] + dist(rbind(iris_data[i,1:4],cluster_2_Sepal$centers[cluster_1_Sepal$cluster[i],]))
  cluster_dist_Sepal[3] = cluster_dist_Sepal[3] + dist(rbind(iris_data[i,1:4],cluster_3_Sepal$centers[cluster_1_Sepal$cluster[i],]))
  cluster_dist_Sepal[4] = cluster_dist_Sepal[4] + dist(rbind(iris_data[i,1:4],cluster_4_Sepal$centers[cluster_1_Sepal$cluster[i],]))
}

cluster_dist_Sepal
which.min(cluster_dist_Sepal)

#Check Classification
table(cluster_1_Sepal$cluster, iris_data$Species)
table(cluster_2_Sepal$cluster, iris_data$Species)
table(cluster_3_Sepal$cluster, iris_data$Species)
table(cluster_4_Sepal$cluster, iris_data$Species)

#plot the clusters
p1<-ggplot(iris_data, aes(Sepal.Length, Sepal.Width, color = cluster_1_Sepal$cluster)) + geom_point() + ggtitle("2 Clusters-Sepal")
p2<-ggplot(iris_data, aes(Sepal.Length, Sepal.Width, color = cluster_2_Sepal$cluster)) + geom_point() + ggtitle("3 Clusters-Sepal")
p3<-ggplot(iris_data, aes(Sepal.Length, Sepal.Width, color = cluster_3_Sepal$cluster)) + geom_point() + ggtitle("4 Clusters-Sepal")
p4<-ggplot(iris_data, aes(Sepal.Length, Sepal.Width, color = cluster_4_Sepal$cluster)) + geom_point() + ggtitle("5 Clusters-Sepal")

grid.arrange(p1,p2,p3,p4, nrow = 2)

#Using just Petal Length and Width
cluster_1_Petal = kmeans(iris_data_scaled[,3:4], 2, nstart = 10)
cluster_2_Petal = kmeans(iris_data_scaled[,3:4], 3, nstart = 10)
cluster_3_Petal = kmeans(iris_data_scaled[,3:4], 4, nstart = 10)
cluster_4_Petal = kmeans(iris_data_scaled[,3:4], 5, nstart = 10)

# total distance between data points and cluster centers for different set of clusters:
cluster_dist_Petal <- rep(0,4)

#Measure sum of distances between each point and its respective cluster center
for (i in 1:nrow(iris_data)) {
  cluster_dist_Petal[1] = cluster_dist_Petal[1] + dist(rbind(iris_data[i,1:4],cluster_1_Petal$centers[cluster_1_Petal$cluster[i],]))
  cluster_dist_Petal[2] = cluster_dist_Petal[2] + dist(rbind(iris_data[i,1:4],cluster_2_Petal$centers[cluster_1_Petal$cluster[i],]))
  cluster_dist_Petal[3] = cluster_dist_Petal[3] + dist(rbind(iris_data[i,1:4],cluster_3_Petal$centers[cluster_1_Petal$cluster[i],]))
  cluster_dist_Petal[4] = cluster_dist_Petal[4] + dist(rbind(iris_data[i,1:4],cluster_4_Petal$centers[cluster_1_Petal$cluster[i],]))
}

cluster_dist_Petal
which.min(cluster_dist_Petal)

#Check Classification
table(cluster_1_Petal$cluster, iris_data$Species)
table(cluster_2_Petal$cluster, iris_data$Species)
table(cluster_3_Petal$cluster, iris_data$Species)
table(cluster_4_Petal$cluster, iris_data$Species)

#plot the clusters
p1<-ggplot(iris_data, aes(Petal.Length, Petal.Width, color = cluster_1_Petal$cluster)) + geom_point() + ggtitle("2 Clusters-Petal")
p2<-ggplot(iris_data, aes(Petal.Length, Petal.Width, color = cluster_2_Petal$cluster)) + geom_point() + ggtitle("3 Clusters-Petal")
p3<-ggplot(iris_data, aes(Petal.Length, Petal.Width, color = cluster_3_Petal$cluster)) + geom_point() + ggtitle("4 Clusters-Petal")
p4<-ggplot(iris_data, aes(Petal.Length, Petal.Width, color = cluster_4_Petal$cluster)) + geom_point() + ggtitle("5 Clusters-Petal")

grid.arrange(p1,p2,p3,p4, nrow = 2)

#Get sum of squares for multiple values of k 
#Elbow Diagram
sum_squares<- sapply(1:10,function(k){kmeans(data[,c(2:4)],k,iter.max=100,nstart=10)$tot.withinss})
plot(1:10,sum_squares)
