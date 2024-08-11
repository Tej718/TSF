#Loading the required packages
library(ggplot2)
library(cluster)


#Load and explore the data set
setwd("C:\\Users\\Tajasa\\OneDrive\\Documents\\Econometrics")
getwd()
iris <- read.csv("Iris.csv")
head(iris)
summary(iris)



#remove the species column 
#as it is a categorical feature 
iris_data <- iris[, -6]
str(iris_data)


#Calculating the total within-cluster sum of squares
wss <- function(k) {
  kmeans(iris_data, k, nstart = 10)$tot.withinss
}

#Computing wss for k=1 to k=10
k.values <- 1:10

#Extract wss for clusters 1 to 10
wss_values <- sapply(k.values, wss)

#Plotting the Elbow method
plot(k.values, wss_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K", 
     ylab = "Total within-clusters sum of squares")       #Hence, clusters = 3



#Applying K-means Clustering
set.seed(123)
km_result <- kmeans(iris_data, centers = 3, nstart = 25)

#View the total no of clusters formed 
km_result$cluster



#adding the cluster results to iris data set
iris_cluster <- iris
iris_cluster$Cluster <- as.factor(km_result$cluster)

#Visualizing the clusters
ggplot(iris_cluster, aes(PetalLengthCm, PetalWidthCm, color = Cluster)) +
  geom_point(size = 3) +
  labs (title = "K-means Clustering with 3 Clusters", 
        x = "Petal Length", 
        y = "Petal Width")


#Validating clusters with True Labels
ggplot(iris_cluster, aes(PetalLengthCm, PetalWidthCm, color = Species)) +
  geom_point (size = 3) +
  labs(title = "Iris Species Distribution", 
       x = "Petal Length (in cm)", 
       y = "Petal Width")
