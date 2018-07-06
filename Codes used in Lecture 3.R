### Time series distance

x1 <- c(1,4,5,4,2,1,1,4)
x2 <- c(2,3,2,2,3,3,3,3)
x3 <- 3+c(4,8,8,9,6,5,5,9)

plot(1:8,x1,type="l",xaxt="n",ylim=c(0,12),xlab="Variables",ylab="Values")
points(1:8,x2,type="l",col=2)
points(1:8,x3,type="l",col=4)
axis(1,at=1:8,labels=c("V1","V2","V3","V4","V5","V6","V7","V8"))
text(c(3,3,3),c(1.5,5.5,10.5),labels=c("x2","x1","x3"),col=c(2,1,4))

### Olive oil distances

dolive2 <- dist(olive,method="euclidean")
dolive1 <- dist(olive,method="manhattan")

# This shows you how similar they are:
plot(dolive1,dolive2,cex=0.3)

# Actually, it makes much more of a difference to scale the data.
solive <- scale(olive)
dolives2 <- dist(solive,method="euclidean")
dolives1 <- dist(solive,method="manhattan")

plot(dolive2,dolives2,cex=0.3) # For example.

# Note that dist produces an object of class dist.
# This is a vector of all distances in one row.
# This is memory efficient, but sometimes it's more 
# useful to have a distance matrix, so that it is 
# easy to look up specific values:

dolivematrix <- as.matrix(dolives2)
# This gives the distance between observations 1 and 2.
dolivematrix[1,2]
# [1] 0.6644688

# A distance matrix can be transformed into a dist-object
# by as.dist.

# The mahalanobis command can only compute a vector of
# Mahalanobis distances, so producing all distances is
# more tedious; here's how to make a distance matrix:

mahalm <- matrix(0,ncol=572,nrow=572)
olivecov <- cov(olive)
molive <- as.matrix(olive)
for (i in 1:572)
  mahalm[i,] <- mahalanobis(molive,molive[i,],olivecov)

# Note that for the Mahalanobis distance it doesn't make
# a difference whether the dataset is scaled or not.
# Still it's quite different from the distance on
# standardised data.
plot(as.dist(mahalm),dolives2,cex=0.3)

### Jaccard and simple matching distance

jveronica <- dist(veronica,method="binary")
smveronica <- dist(veronica,method="manhattan")/p
#### p = 583


plot(jveronica,smveronica,cex=0.3)

### Correlation dissimilarity

corp05 <- cor(p05)
cordistp05 <- 0.5-corp05/2

### Multidimensional scaling

mdsparties <- cmdscale(cordistp05,k=2)
# k is the number of dimensions here.

plot(mdsparties,type="n")
text(mdsparties,labels=dimnames(p05)[[2]])

mdsveronica <- cmdscale(jveronica,k=2)
plot(mdsveronica)

### Adjusted Rand index

solive <- scale(olive)
olive3 <- kmeans(olive,3,nstart=100)
olive3s <- kmeans(solive,3,nstart=100)

adjustedRandIndex(olive3$cluster,olive3s$cluster)
# 0.4587804, these are somewhat different.

adjustedRandIndex(olive3$cluster,oliveoil$macro.area)
# 0.3182057

adjustedRandIndex(olive3s$cluster,oliveoil$macro.area)
# 0.448355, both OK but not great, with scaling clearly better

# The Sugar and James index suggests a higher number of clusters.
olive20 <- kmeans(solive,20,nstart=100) 
adjustedRandIndex(olive20$cluster,oliveoil$macro.area)
# 0.166162, not helpful.

### Hierarchical clustering


# Average Linkage
plantclust <- hclust(jveronica,method="average")

# Plot the dendrogram:
plot(plantclust) 

# 8 looks like a good number of clusters.
plantclust8 <- cutree(plantclust,8)

# Visualisation using MDS:
plot(mdsveronica,col=plantclust8,pch=clusym[plantclust8])
# This looks good.

# Single Linkage 
plantclusts <- hclust(jveronica,method="single")
plot(plantclusts) # 8 looks still OK
plantclusts8 <- cutree(plantclusts,8)
plot(mdsveronica,col=plantclusts8,pch=clusym[plantclusts8])
# The same as before.

# Complete Linkage
plantclustc <- hclust(jveronica,method="complete")
plot(plantclustc) # Could use 8 or 9.
plantclustc8 <- cutree(plantclustc,8)
plot(mdsveronica,col=plantclustc8,pch=clusym[plantclustc8])
# The same as before.

# I decided to use the Jaccard distance also for AFLP bands/genes:
vveronica <- dist(t(veronicam),method="binary")
varclust <- hclust(vveronica,method="average")
# As a clustering this is pretty messy, but still it
# can be used to impose an order of genes.

# This submits the two dendrograms to the heatmap:
heatmap(veronicam,Rowv=as.dendrogram(plantclust),
        Colv=as.dendrogram(varclust),
        col=grey(seq(1,0,-0.01)))

# Average Linkage
geyclust <- hclust(dist(sgeyser),method="average")
plot(geyclust) 
# 4 looks like a good K, will isolate one outlier
geyclust4 <- cutree(geyclust,4)
plot(sgeyser,col=geyclust4,pch=clusym[geyclust4])

# Single Linkage
geyclusts <- hclust(dist(sgeyser),method="single")
plot(geyclusts) 
# 7 looks like a good K
geyclusts7 <- cutree(geyclusts,7)
plot(sgeyser,col=geyclusts7,pch=clusym[geyclusts7])

# Complete Linkage
geyclustc <- hclust(dist(sgeyser),method="complete")
plot(geyclustc) 
# 5 looks like a good K
geyclustc5 <- cutree(geyclustc,5)
plot(sgeyser,col=geyclustc5,pch=clusym[geyclustc5])

### Ward's method

# kmeans with 5 clusters
set.seed(12345)
kmbundestag5 <- kmeans(p05,5,nstart=100)

# Ward's method
wbundestag <- hclust(dist(p05),method="ward.D2")
# plot(wbundestag) # Dendrogram not shown
# Same as wbundestag <- agnes(p05,method="ward")

# With K=5:
wbundestag5 <- cutree(wbundestag,5)

table(kmbundestag5$cluster,wbundestag5)
#   wbundestag5
#     1  2  3  4  5
#  1 27  0  0 55  0
#  2 72  0  0  0  0
#  3  0  0  0 16 22
#  4  8  0 36  0  0
#  5  0 63  0  0  0
# Fairly different.

adjustedRandIndex(kmbundestag5$cluster,wbundestag5)
# [1] 0.6362054

library(fpc)
# This can be used to compute S for any clustering:
kmb <- cluster.stats(dist(p05),kmbundestag5$cluster)
kmb$within.cluster.ss
# 1.319956
# This is the same as kmbundestag5$tot.withinss
wmb <- cluster.stats(dist(p05),wbundestag5)
wmb$within.cluster.ss
# 1.534854
# Quite a bit worse.