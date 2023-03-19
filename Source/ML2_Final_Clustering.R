setwd("/Users/chethammer/Documents/School/R Studio/Data")


#Load the packages
library(openxlsx)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(ggplot2)    # helper for visualization


#Read Travel Review dataset
travel = read.xlsx("/Users/chethammer/Documents/School/R Studio/Data/Travel_Review.xlsx", na.strings = "NA")


# check missing values

colSums(is.na(travel))

travel$Churches[is.na(travel$Churches)] = median(travel$Churches, na.rm=TRUE)
travel$Resorts[is.na(travel$Resorts)] = median(travel$Resorts, na.rm=TRUE)
travel$Beaches[is.na(travel$Beaches)] = median(travel$Beaches, na.rm=TRUE)
travel$Parks[is.na(travel$Parks)] = median(travel$Parks, na.rm=TRUE)
travel$Theatres[is.na(travel$Theatres)] = median(travel$Theatres, na.rm=TRUE)
travel$Museums[is.na(travel$Museums)] = median(travel$Museums, na.rm=TRUE)
travel$Malls[is.na(travel$Malls)] = median(travel$Malls, na.rm=TRUE)
travel$Zoo[is.na(travel$Zoo)] = median(travel$Zoo, na.rm=TRUE)
travel$Restaurants[is.na(travel$Restaurants)] = median(travel$Restaurants, na.rm=TRUE)
travel$Pubs_Bars[is.na(travel$Pubs_Bars)] = median(travel$Pubs_Bars, na.rm=TRUE)


travel$LocalServices = as.numeric(travel$LocalServices)
travel$LocalServices[is.na(travel$LocalServices)] = median(travel$LocalServices, na.rm=TRUE)


travel$Burger_PizzaShops[is.na(travel$Burger_PizzaShops)] = median(travel$Burger_PizzaShops, na.rm=TRUE)
travel$Hotels_OtherLodgings[is.na(travel$Hotels_OtherLodgings)] = median(travel$Hotels_OtherLodgings, na.rm=TRUE)
travel$JuiceBars[is.na(travel$JuiceBars)] = median(travel$JuiceBars, na.rm=TRUE)
travel$ArtGalleries[is.na(travel$ArtGalleries)] = median(travel$ArtGalleries, na.rm=TRUE)
travel$DanceClubs[is.na(travel$DanceClubs)] = median(travel$DanceClubs, na.rm=TRUE)
travel$Gyms[is.na(travel$Gyms)] = median(travel$Gyms, na.rm=TRUE)
travel$Bakeries[is.na(travel$Bakeries)] = median(travel$Bakeries, na.rm=TRUE)
travel$Cafes[is.na(travel$Cafes)] = median(travel$Cafes, na.rm=TRUE)
travel$ViewPoints[is.na(travel$ViewPoints)] = median(travel$ViewPoints, na.rm=TRUE)
travel$Monuments[is.na(travel$Monuments)] = median(travel$Monuments, na.rm=TRUE)
travel$Gardens[is.na(travel$Gardens)] = median(travel$Gardens, na.rm=TRUE)
travel$BeautySpas[is.na(travel$BeautySpas)] = median(travel$BeautySpas, na.rm=TRUE)
travel$Swimming.Pools[is.na(travel$Swimming.Pools)] = median(travel$Swimming.Pools, na.rm=TRUE)


colSums(is.na(travel))

str(travel)

set.seed(123)
#scaling the dataset
travel3 <- scale(travel[,2:25])

#find optimal k
# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(travel3, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
library(purrr)
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


# wss
fviz_nbclust(travel3, kmeans, method="wss")
#6,9

fviz_nbclust(travel3, kmeans, method="silhouette")
#2

gap_stat = clusGap(travel3, FUN=kmeans, nstart=25, K.max=10,B=10)
print(gap_stat, method = "firstmax") 
fviz_gap_stat(gap_stat)
#10


# first kmeans model
set.seed(123)
km1 = kmeans(travel3, centers=6, nstart=25)
km1

fviz_cluster(km1,data=travel3)



# second kmeans model
km2 = kmeans(travel3, centers=9, nstart=25)
km2

fviz_cluster(km2,data=travel3)



# third kmeans model
km3 = kmeans(travel3, centers=10, nstart=25)
km3

fviz_cluster(km3,data=travel3)



# fourth kmeans model
km4 = kmeans(travel3, centers=4, nstart=25)
km4

fviz_cluster(km4,data=travel3)


# fifth kmeans model
km5 = kmeans(travel3, centers=5, nstart=25)
km5

fviz_cluster(km5,data=travel3)


# sixth kmeans model
km6 = kmeans(travel3, centers=3, nstart=25)
km6

fviz_cluster(km6,data=travel3)


#Extracting clusters into original data
travelclus <- cbind(travel, cluster=km4$cluster)
head(travelclus)

library(dplyr)
count(travelclus, 'cluster')

#profile clusters taking mean of the numerical variables
aggregate(x=travelclus[2:25], 
          by=list(travelclus$cluster), 
          FUN=mean)

#and proportions of categorical variables
table =as.table(table(travelclus$cluster, travelclus$UserID))
table



#use the NbClust() function
#Use distance=euclidean or manhattan
#use diss= distance matrix you want to use a distance matrix (use distance=NULL if diss=distance matrix)
#use min.nc or max.nc for number of clusters (min or max)
#method="kmeans" or "ward.D2" or "complete" or "average" etc. 

library(NbClust)
library(factoextra)
#kmeans euclidean
nb1<-NbClust(data = travel3, distance = "euclidean",
            min.nc = 2, max.nc = 15, method = "kmeans")

fviz_nbclust(nb1)
#3
#kmeans manhattan
nb2<-NbClust(data = travel3, distance = "manhattan",
            min.nc = 2, max.nc = 15, method = "kmeans")

fviz_nbclust(nb2)
#3
#kmeans max
nb3<-NbClust(data = travel3, distance = "maximum",
            min.nc = 2, max.nc = 15, method = "kmeans")

fviz_nbclust(nb3)


#Cluster validation method
#Cluster profiles are best

# Compute clValid
install.packages("clValid")
library(clValid)
clmethods <- c("hierarchical","kmeans","pam")
intern <- clValid(travel3, nClust = 2:10,
                  clMethods = clmethods, validation = "internal")
# Summary
summary(intern)


#Visualize results in a scatter plot
fviz_cluster(list(data = travel3, cluster = intern),
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "#00FF00"),
             ellipse.type = "convex", # Concentration ellipse
             repel = FALSE, # Allow label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())



#3. Hierarchical Clustering method
#For example, given a distance matrix “distance” generated by the function dist()
#the base R function hclust() can be used to create the hierarchical tree
#do not use the ward.D method (it does not correctly implement Ward's distance)
distance <- get_dist(travel3)
hclus <- hclust(d=distance, method="ward.D2")

#dendrogram: cex controls indicates label size
library("factoextra")
fviz_dend(hclus, cex = 0.5)

#lets cut the tree into four groups
grp <- cutree(hclus, k=4)

#number of members in each cluster
table(grp)

# Cut in 4 groups and color by groups
fviz_dend(hclus, k = 4, 
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

#Visualize results in a scatter plot
fviz_cluster(list(data = travel3, cluster = grp),
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "#00FF00"),
             ellipse.type = "convex", # Concentration ellipse
             repel = FALSE, # Allow label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())

#Cluster R package
#Easy to perform hierarchical clustering using function agnes() for agglomerative 
#or diana() for divisive clustering

# Agglomerative Method (Hierarchical Clustering)
res.agnes <- agnes(x = travel3, # data matrix
                   stand = TRUE, # Standardize the data
                   metric = "euclidean", # metric for distance matrix
                   method = "ward" # Linkage method
)
# Divisive Analysis Clustering (Hierarchical Clustering)
res.diana <- diana(x = travel3, # data matrix
                   stand = TRUE, # standardize the data
                   metric = "euclidean" # metric for distance matrix
)

fviz_dend(res.agnes, cex = 0.6, k = 5)
fviz_dend(res.diana, cex = 0.6, k = 5)







#To compute a partitioning clustering, such as k-means clustering with k = 3, type this:
  
  # K-means clustering
km.res <- eclust(travel3, "kmeans", k = 4, nstart = 25, graph = FALSE)
# Visualize k-means clusters
fviz_cluster(km.res, geom = "point", ellipse.type = "norm",
             palette = "jco", ggtheme = theme_minimal())



#To compute a hierarchical clustering, use this:
  
  # Hierarchical clustering
hc.res <- eclust(travel3, "hclust", k = 3, hc_metric = "euclidean", 
                   hc_method = "ward.D2", graph = FALSE)

# Visualize dendrograms
fviz_dend(hc.res, show_labels = FALSE,
          palette = "jco", as.ggplot = TRUE)



#append cluster labels to original data
final_data <- cbind(travel, cluster = grp)

#display first six rows of final data
head(final_data)

#find mean values for each cluster
aggregate(final_data, by=list(cluster=final_data$cluster), mean)
