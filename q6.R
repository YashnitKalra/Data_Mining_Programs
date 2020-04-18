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

table(iris$Species,fitK$cluster)
ggplot(iris, aes(Petal.Length,Petal.Width,color=iris$Species))+geom_point()
ggplot(iris, aes(Petal.Length,Petal.Width,color=fitK$cluster))+geom_point()

# hierarchial clustering
d=dist(irisScaled) # distance matrix: matrix of distance b/w every point to any other
fitH=hclust(d,"ward.D2")
# ?hclust
rect.hclust(fitH,k=3,border = 'red')
cluster=cutree(fitH,3)
cluster
plot(irisScaled,col=cluster)
table(iris$Species,cluster)
ggplot(iris, aes(Petal.Length, Petal.Width, color = iris$Species)) +
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = cluster) +
  scale_color_manual(values = c('black', 'red', 'green'))
# if inner color and outer color does not match they are clustered incorrectly

#dbScan
kNNdistplot(irisScaled,k=3) # to decide value of eps 
abline(h=0.7,col='red',lty=2)
fitD=dbscan(irisScaled,eps=0.7,minPts=5) # minimum pts that form a cluster
plot(irisScaled,col=fitD$cluster)
table(iris$Species,fitD$cluster)