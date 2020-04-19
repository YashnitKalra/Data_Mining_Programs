library(ggplot2)
library(dbscan)

plot(iris$Sepal.Length,iris$Sepal.Width)
# str(iris)
# normalized data
irisScaled=scale(iris[,-5])
summary(irisScaled)

# k-means clustering
fitK=kmeans(irisScaled,3) # dataset and number of clusters
fitK
fitK$size # size of each cluster
fitK$cluster # cluster assigned to each item in dataset
# str(fitK)
plot(irisScaled,col=fitK$cluster)

# choosing k
#k=list()
#for(i in 1:10){
#  k[[i]]=kmeans(irisScaled,i)
#}
# k
#between_totss=list()
#for(i in 1:10){
#  between_totss[[i]]=k[[i]]$betweenss/k[[i]]$totss
#}
#plot(1:10,between_totss,type = 'b',ylab='between_ss/tot_ss',xlab = 'Cluster(k)')
#for(i in 1:4){
#  plot(irisScaled,col=k[[i]]$cluster)
#}

table(iris$Species,fitK$cluster) # shows how many items of a each species belong to each cluster
ggplot(iris, aes(Petal.Length,Petal.Width,color=iris$Species))+geom_point()
ggplot(iris, aes(Petal.Length,Petal.Width,color=fitK$cluster))+geom_point()

# hierarchial clustering
d=dist(irisScaled) # distance matrix: matrix of distance b/w every point to any other
fitH=hclust(d,"ward.D2") # in ward.D2 method dissimilarities are squared before cluster updating
# ?hclust
plot(fitH)
rect.hclust(fitH,k=3,border = 'red') # draws rectangle around branches of dendogram highlighting the corresponding clusters
cluster=cutree(fitH,3) # cuts a tree into desired number of groups
cluster
plot(irisScaled,col=cluster)
wardtable(iris$Species,cluster)
ggplot(iris, aes(Petal.Length, Petal.Width, color = iris$Species)) +
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = cluster) +
  scale_color_manual(values = c('black', 'red', 'green'))
# if inner color and outer color does not match they are clustered incorrectly

#dbScan
# Method for determining the optimal eps value
# The method proposed here consists of computing the he k-nearest neighbor distances in a matrix of points.
# The idea is to calculate, the average of the distances of every point to its k nearest neighbors. The value of k will be specified by the
# user and corresponds to MinPts.
# Next, these k-distances are plotted in an ascending order. The aim is to determine the “knee”, which corresponds to the
# optimal eps parameter.
# A knee corresponds to a threshold where a sharp change occurs along the k-distance curve.

kNNdistplot(irisScaled,k=3) # to decide value of eps
# eps => epsilon neighborhood: Radius of our neighborhoods around a data point.
abline(h=0.7,col='red',lty=2) # in graph we see at 0.7 sharp changes occur so we consider 0.7 as knee value
fitD=dbscan(irisScaled,eps=0.7,minPts=5) # minimum pts that form a cluster
plot(irisScaled,col=fitD$cluster)
table(iris$Species,fitD$cluster)
