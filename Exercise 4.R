###############################################
########### STAT3019 Exercise 4 ###############
###############################################


## Q1
## a. Veronica data
### Jaccard and simple matching distance
jveronica <- dist(veronica,method="binary")
smveronica <- dist(veronica,method="manhattan")/p  ## p=583
mdsveronica <- cmdscale(jveronica,k=2)   # jeronica
plot(mdsveronica, main="Multidimensional Scaling graph of k=2 with Jaccard distance")
smmdsveronica <- cmdscale(smveronica, k=2) # Simple matching
plot(smmdsveronica, main = "Multidimensional Scaling graph of k=2 with Simple Matching Distance")
## b. Geyser data
eugeyser<- dist(sgeyser, method = "euclidean")  
mgeyser <- dist(sgeyser, method = "manhattan")
str(geyser)   # 299 obs. of 2 variables
mahalm <- matrix(0,ncol=299,nrow=299)
geysercov <- cov(geyser)
mgeyser <- as.matrix(geyser)
for (i in 1:299)
  mahalm[i,] <- mahalanobis(mgeyser,mgeyser[i,],geysercov)
plot(as.dist(mahalm), eugeyser, main = "Geyser dataset comparing mahalanobis and Euclidean",
     xlab="Mahalanobis Distance", ylab = "Euclidean distance")
plot(as.dist(mahalm), mgeyser, main = "Geyser dataset comparing mahalanobis and Manhattan",
     xlab = "Mahalanobis Distance", ylab = "Manhattan distance")
plot(eugeyser, mgeyser, main = "Geyser dataset comparing Euclidean and Manhattan",
     xlab = "Euclidean Distance", ylab = "Manhattan distance")




###### Q2.
### Adjusted Rand index
library(mclust)
solive <- scale(olive)
olive9<- kmeans(olive, centers = 9, nstart = 100)
olive.wt1 <- kmeans(olive[,-1],centers=9,nstart=100)
adjustedRandIndex(olive9$cluster,olive.wt1$cluster)
ARI.olive<- array(NA,8)
i = 0

while (i < 8){
  i= i+1
  olive.wt <- kmeans(olive[,-i], centers = 9, nstart = 100)
  ARI.olive[i] <- adjustedRandIndex(olive9$cluster, olive.wt$cluster)
}


### b.
olive9s <- kmeans(solive,9,nstart=100)
ARI.solive<- array(NA,8)
i = 0
while (i < 8){
  i= i+1
  olive.wt <- kmeans(solive[,-i], centers = 9, iter.max=20, nstart = 100)
  ARI.solive[i] <- adjustedRandIndex(olive9s$cluster, olive.wt$cluster)
}

#### FUNCTION
ARI <- function(data, K) {
  olive9 <- kmeans(data, centers = K, nstart=100)
  ARI.olive<- array(NA,8)
  
  i = 0
  
  while (i < 8){
    i= i+1
    olive.wt <- kmeans(olive[,-i], centers = K, nstart = 100)
    ARI.olive[i] <- adjustedRandIndex(olive9$cluster, olive.wt$cluster)
  }
}

ARI(data=olive,9)


#### Q5.
single.veronica<- hclust(dist(veronica), method = "single")
plot(single.veronica)
single.veronica.2<- cutree(single.veronica, k=2)
plot(mdsveronica, col= single.veronica.2, pch=clusym[single.veronica.2])


complete.veronica<- hclust(dist(veronica), method = "complete")
plot(complete.veronica)
complete.veronica.2<- cutree(complete.veronica, k=2)
plot(mdsveronica, col= complete.veronica.2, pch=clusym[complete.veronica.2])

average.veronica<- hclust(dist(veronica), method = "average")
plot(average.veronica)
average.veronica.2<- cutree(average.veronica, k=2)
plot(mdsveronica, col= average.veronica.2, pch=clusym[average.veronica.2])

adjustedRandIndex(single.veronica.2, complete.veronica.2)

### loop
single.veronica<- hclust(dist(veronica), method = "single")
complete.veronica<- hclust(dist(veronica), method = "complete")
average.veronica<- hclust(dist(veronica), method = "average")
aveARI<-c()
K=0
while (K < 20){
  K=K+1
single.veronica.k<- cutree(single.veronica, k=K)
complete.veronica.k<- cutree(complete.veronica, k=K)
average.veronica.k<- cutree(average.veronica, k=K)
x1<- adjustedRandIndex(single.veronica.k, complete.veronica.k)
x2<- adjustedRandIndex(single.veronica.k, average.veronica.k)
x3<- adjustedRandIndex(complete.veronica.k, average.veronica.k)
aveARI[K] <- sum(x1,x2,x3)/3
}
print(aveARI)
