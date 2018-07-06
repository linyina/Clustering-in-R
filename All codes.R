# I collect loading all required libraries in the beginning
library(MASS)
library(flexclust)
library(fpc)
library(pdfCluster)
library(mclust)
library(cluster)

### Geyser data

data(geyser)
# Here's how it looks like:
str(geyser)
#'data.frame':	299 obs. of  2 variables:
# $ waiting : num  80 71 57 80 75 77 60 86 77 56 ...
# $ duration: num  4.02 2.15 4 4 4 ...

plot(geyser)


### Bundestag data

p05 <- bundestag(2005)
str(p05)
# How the object looks like:
# num [1:299, 1:5] 0.391 0.362 0.363 0.376 0.415 ...
# - attr(*, "dimnames")=List of 2
#  ..$ : chr [1:299] "Flensburg - Schleswig" "Nordfriesland - 
# Dithmarschen Nord" "Steinburg - Dithmarschen Sued" 
# "Rendsburg-Eckernfoerde" ...
#  ..$ : chr [1:5] "SPD" "UNION" "GRUENE" "FDP" ...

# federal states:
state <- bundestag(2005, state=TRUE)

# ewb takes values "East", "West", "Berlin" as explained:
ewb <- rep("West",299)
ewb[state=="Berlin"] <- "Berlin"
ewb[state %in% c("Brandenburg","Mecklenburg-Vorpommern","Sachsen",
                 "Sachsen-Anhalt","Thueringen")] <- "East"

# Plotting
pairs(p05,xlim=c(0,0.6),ylim=c(0,0.6),cex=0.5)
# Note the xlim, ylim parameters. All values of all variables
# have the same meaning, so fixing the x and y value range for 
# all variables simultaneously will show which parties are 
# big and which are small.
# cex=0.5 makes plot symbols smaller.

### Olive Oil data

data(oliveoil)
# Look at object:
str(oliveoil)
# 'data.frame':	572 obs. of  10 variables:
#  $ macro.area : Factor w/ 3 levels "South","Sardinia",..: 1 1 1 1 1 1 1 1 1 1 ...
#  $ region     : Factor w/ 9 levels "Apulia.north",..: 1 1 1 1 1 1 1 1 1 1 ...
#  $ palmitic   : int  1075 1088 911 966 1051 911 922 1100 1082 1037 ...
#  $ palmitoleic: int  75 73 54 57 67 49 66 61 60 55 ...
#  $ stearic    : int  226 224 246 240 259 268 264 235 239 213 ...
#  $ oleic      : int  7823 7709 8113 7952 7771 7924 7990 7728 7745 7944 ...
#  $ linoleic   : int  672 781 549 619 672 678 618 734 709 633 ...
#  $ linolenic  : int  36 31 31 50 50 51 49 39 46 26 ...
#  $ arachidic  : int  60 61 63 78 80 70 56 64 83 52 ...
#  $ eicosenoic : int  29 29 29 35 46 44 29 35 33 30 ...

# The chemical variables:
olive <- oliveoil[,3:10]
# Plot:
pairs(olive,cex=0.3)

### Veronica data

veronica <- read.table("mydir/veronica.dat")
str(veronica)
#'data.frame':	207 obs. of  583 variables:
# $ V1  : int  0 0 0 1 0 0 0 0 0 1 ...
# $ V2  : int  0 0 0 0 0 1 0 0 0 0 ...
# $ V3  : int  1 1 0 1 0 0 0 0 0 0 ...
# $ V4  : int  0 0 0 0 0 0 0 0 0 0 ...
# (...)

# Some things can better be done on matrices:
veronicam <- as.matrix(veronica)

# A pairs plot isn't suitable for 0-1 data, and also not
# for 583 variables. 
# A better plot is a heatmap, that plots value 1 in black 
# and zero in white.

heatmap(veronicam,Rowv=NA,Colv=NA,col=grey(seq(1,0,-0.01)))
# This is black for 1-entries
# rows are plants, columns are variables

# This is how I generated the dataset.
# You actually don't need to do this.


### Artificial dataset 1

# You don't need to run this
# library(sn)
# set.seed(665544)
# v1 <- c(rnorm(50,0,1), rsn(70,5,1,8), rnorm(30,6,1))
# v2 <- c(rnorm(50,0,1), rsn(70,0,1,8), 8+rt(30,5))
# clusterdata1 <- cbind(v1,v2)

# Here is how to load the dataset from directory mydir.
clusterdata1 <- as.matrix(read.table("clusterdata1.dat"))
str(clusterdata1)
# num [1:150, 1:2] -0.982 0.378 0.547 1.433 0.75 ...
# - attr(*, "dimnames")=List of 2
#  ..$ : NULL
#  ..$ : chr [1:2] "v1" "v2"

plot(clusterdata1)

### Artificial dataset 2

# You don't need to run this
# set.seed(77665544)
# x1 <- rnorm(20)
# y1 <- rnorm(20)
# x2 <- rnorm(20,mean=10)
# y2 <- rnorm(20)
# x3 <- runif(100,-20,30)
# y3 <- runif(100,20,40)
# clusterdata2 <- cbind(c(x1,x2,x3),c(y1,y2,y3))

# Here is how to load the dataset from directory mydir.
clusterdata2 <- as.matrix(read.table("clusterdata2.dat"))
str(clusterdata2)
# num [1:140, 1:2] -1.694 0.494 -0.753 -0.16 -0.375 ...

plot(clusterdata2)

### K-means examples: artificial dataset 1

set.seed(665544)
c1k3 <- kmeans(clusterdata1,centers=3,nstart=100)
plot(clusterdata1,col=c1k3$cluster,pch=clusym[c1k3$cluster])
points(c1k3$centers,pch="M",cex=2,col=4)


### K-means examples: Geyser

set.seed(12345)
geyserk2 <- kmeans(geyser,2,nstart=100)


plot(geyser,col=geyserk2$cluster,pch=clusym[geyserk2$cluster])


geyserk3 <- kmeans(geyser,3,nstart=100)


plot(geyser,col=geyserk3$cluster,pch=clusym[geyserk3$cluster])


plot(geyser,xlim=c(40,110),ylim=c(0,70))

sgeyser <- scale(geyser)

geysersk2 <- kmeans(sgeyser,2,nstart=100)


plot(sgeyser,col=geysersk2$cluster,pch=clusym[geysersk2$cluster])

geysersk3 <- kmeans(sgeyser,3,nstart=100)


plot(sgeyser,col=geysersk3$cluster,pch=clusym[geysersk3$cluster])

### K-means examples: Bundestag

set.seed(1234567)
bundestagk5 <- kmeans(p05,5,nstart=100)

pairs(p05,xlim=c(0,0.6),ylim=c(0,0.6),cex=0.7,col=bundestagk5$cluster,pch=clusym[bundestagk5$cluster])

bundestagk5$centers

table(bundestagk5$cluster,ewb)

table(bundestagk5$cluster,state)

### K-means examples: artificial data 2

c2 <- kmeans(clusterdata2,3,nstart=100)$cluster
plot(clusterdata2,col=c2,pch=clusym[c2])

### Plot of two Gaussian densities

xpoints <- seq(-4,4,by=0.01)
norm1 <- dnorm(xpoints,mean=-1,sd=1)
norm2 <- dnorm(xpoints,mean=1,sd=1)

# Plotting densities:
plot(xpoints,norm1,type="l",ylab="density")
points(xpoints,norm2,type="l")

# red line, and means of distributions
lines(c(0,0),c(0,0.4),col=2)
points(1,0,pch="X",col=4)
points(-1,0,pch="X",col=4)

# Computations for truncated distributions.
# See https://en.wikipedia.org/wiki/Truncated_normal_distribution
# for formulae.

# Probability of being below zero for left Gaussian
prob1 <- pnorm(0,-1,1) 
# Probability of being below zero for right Gaussian
prob2 <- pnorm(0,1,1)

# Expected value of Gaussian with mean -1, truncated at 0.
te1 <- -1-dnorm(1)/pnorm(1)
# Expected value of Gaussian with mean 1, truncated at 0.
te2 <- 1-dnorm(-1)/pnorm(-1)
# Expected value for left cluster combines the two according to 
# their probabilities for their controbutions to the left cluster.
ecluster1 <- prob1*te1+prob2*te2
# Symmetric for right cluster.
ecluster2 <- -ecluster1

# Plot cluster means
points(ecluster1,0,pch="M",col=3)
points(ecluster2,0,pch="M",col=3)

### Estimating the number of clusters


set.seed(123456)
# K.max=20 indicates the maximum number of clusters; B=100 indicates 
# 100 simulations from uniform distribution; d.power=2 indicates squared
# Euclidean distances (as in the original paper and in class),
# spaceH0="original" specifies the uniform distribution to be simulated
# from in the way introduced in class and as "method (a)" in the original
# paper. This is *NOT* the default; the default spaceH0 = "scaledPCA" is
# probably better in most cases but far more difficult to explain in class
# (at least for those who don't know PCA).
cg1 <- clusGap(clusterdata1,kmeans,10,B=100,d.power=2,spaceH0="original",nstart=100)
print(cg1,method="Tibs2001SEmax")
# B=100 simulated reference sets, k = 1..10; spaceH0="original"
#  --> Number of clusters (method 'Tibs2001SEmax', SE.factor=1): 3

plot(cg1)
# Values of gap

plot(1:10,exp(cg1$Tab[,1]),xlab="k",ylab="S_k",type="l")
# Values of S_k; cg1$Tab[,1] has values of log(S_k), therefore the exp.

plot(1:10,cg1$Tab[,1],xlab="k",ylab="log S_k",type="l")
points(1:10,cg1$Tab[,2],xlab="k",ylab="log S_k",type="l",lty=2)
legend(6,6.5,c("log S_k in data","E(log S_k) uniform"),lty=1:2) 
# Values of log(S_k) and its expectation under uniform distribution 
# The latter are in cg1$Tab[,2].



set.seed(76543)
# Use the scaled version of the dataset!
cgg <- clusGap(sgeyser,kmeans,10,B=100,d.power=2,spaceH0="original",nstart=100)
print(cgg,method="Tibs2001SEmax")
# B=100 simulated reference sets, k = 1..10; spaceH0="original"
#  --> Number of clusters (method 'Tibs2001SEmax', SE.factor=1): 3
plot(cgg)
# Values of gap

plot(1:10,exp(cgg$Tab[,1]),xlab="k",ylab="S_k",type="l")
# Values of S_k

plot(1:10,cgg$Tab[,1],xlab="k",ylab="log S_k",type="l")
points(1:10,cgg$Tab[,2],xlab="k",ylab="log S_k",type="l",lty=2)
legend(6,5,c("log S_k in data","E(log S_k) uniform"),lty=1:2) 
# Values of log(S_k) and its expectation under uniform distribution 



# Unfortunately, the "clusGap"-function doesn't give out a clustering,
# so this has to be computed afterwards again.
geyser3 <- kmeans(sgeyser,3,nstart=100)

plot(sgeyser,col=geyser3$cluster,pch=clusym[geyser3$cluster])

set.seed(12345)
cgp05 <- clusGap(p05,kmeans,10,B=100,d.power=2,spaceH0="original",nstart=100)
print(cgp05,method="Tibs2001SEmax")
# B=100 simulated reference sets, k = 1..10; spaceH0="original"
#  --> Number of clusters (method 'Tibs2001SEmax', SE.factor=1): 7
plot(cgp05)
# Gap values

plot(1:10,exp(cgp05$Tab[,1]),xlab="k",ylab="S_k",type="l")
# S_k values

plot(1:10,cgp05$Tab[,1],xlab="k",ylab="log S_k",type="l")
points(1:10,cgp05$Tab[,2],xlab="k",ylab="log S_k",type="l",lty=2)
legend(6,5,c("log S_k in data","E(log S_k) uniform"),lty=1:2) 
# log S_k values and expectation under uniform

# Re-compute 7-means
p057 <- kmeans(p05,7,nstart=100)

pairs(p05,col=p057$cluster,cex=0.5)

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

# The gap statistic suggests a higher number of clusters.
solive <- scale(olive)
cgolive <- clusGap(solive,kmeans,20,B=100,d.power=2,spaceH0="original",nstart=100)
print(cgolive,method="Tibs2001SEmax")
# --> Number of clusters (method 'Tibs2001SEmax', SE.factor=1): 14
kmolive14 <- kmeans(solive,14,nstart=100)
adjustedRandIndex(kmolive14$cluster,oliveoil$region)
# [1] 0.5188687
adjustedRandIndex(kmolive14$cluster,oliveoil$macro.area)
# [1] 0.2436688


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

### PAM

bundestagk5 <- kmeans(p05,5,nstart=100)

# By default, if pam is called with a dataset that is not a 
# dist-object, the Euclidean distance is used:
bundestagp5 <- pam(p05,5)

# There's a bit of a difference between k-means and pam
# based on Euclidean distances 
adjustedRandIndex(bundestagk5$cluster,bundestagp5$cluster)
[1] 0.7249983

# Alternatively, pam can be started with a dist-object, which allows
# computing it with the Manhattan-distance:
p05manhattan <- dist(p05,method="manhattan")
bundestagp5m <- pam(p05manhattan,5)

# Somewhat surprisingly, with Manhattan distance the clustering
# is more similar to K-means than to PAM with Euclidean
# distance. This is atypical.  
adjustedRandIndex(bundestagp5$cluster,bundestagp5m$cluster)
[1] 0.6607979

adjustedRandIndex(bundestagk5$cluster,bundestagp5m$cluster)
[1] 0.8863854

# One can visualise this with an MDS-plot based on the
# Manhattan distance.
mdsp05m <- cmdscale(p05manhattan,2)

plot(mdsp05m,col=bundestagk5$cluster,pch=clusym[bundestagk5$cluster])
plot(mdsp05m,col=bundestagp5$cluster,pch=clusym[bundestagp5$cluster])
plot(mdsp05m,col=bundestagp5m$cluster,pch=clusym[bundestagp5m$cluster])

### Average Silhouette Width

pasw <- NA
pclusk <- list()
psil <- list()

# Look at K between 2 and 30:
for (k in 2:30){
  # PAM clustering:
  pclusk[[k]] <- pam(p05manhattan,k) 
  # Computation of silhouettes:
  psil[[k]] <- silhouette(pclusk[[k]],dist=p05manhattan) 
  # The silhouette function produces a lot of information,
  # from which the ASW needs to be extracted.
  pasw[k] <- summary(psil[[k]])$avg.width
}

# Plot the ASW-values against K:
plot(1:30,pasw,type="l",xlab="Number of clusters",ylab="ASW")

# Result in numbers:
# > pasw
# [1]        NA 0.4867459 0.4228157 0.4265560 0.3779154 0.3345530 
# 0.3131574
#  [8] 0.3254449 0.3177023 0.3185734 0.3036558 0.3130183 0.3039691 
# 0.3157744
# [15] 0.3235948 0.3151002 0.3000481 0.2983881 0.3003727 0.3017265 
# 0.3043202
# [22] 0.3101771 0.3085357 0.3115082 0.3091192 0.2995540 0.3008825 
# 0.2839130
# [29] 0.2832789 0.2888274
# Best value at K=2.

# MDS-plot:
plot(mdsp05m,col=pclusk[[2]]$cluster,pch=clusym[pclusk[[2]]$cluster])

# Silhouette plots for 2 and 5 clusters:
plot(psil[[2]])
plot(psil[[5]])

# Jaccard-distance:
jveronica <- dist(veronica,method="binary")

# PAM:
pasw <- NA
pclusk <- list()
psil <- list()
for (k in 2:30){
  pclusk[[k]] <- pam(jveronica,k)
  psil[[k]] <- silhouette(pclusk[[k]])
  pasw[k] <- summary(psil[[k]])$avg.width
}
plot(1:30,pasw,type="l",xlab="Number of clusters",ylab="ASW")
pasw
# (gives the numbers; there's a maximum at K=7,
# ASW=0.5386146

# Average Linkage
plantclust <- hclust(jveronica,method="average")

tasw <- NA
tclusk <- list()
tsil <- list()
for (k in 2:30){
  tclusk[[k]] <- cutree(plantclust,k)
  tsil[[k]] <- silhouette(tclusk[[k]],dist=jveronica)
  tasw[k] <- summary(silhouette(tclusk[[k]],dist=jveronica))$avg.width
}
plot(1:30,tasw,type="l",xlab="Number of clusters",ylab="ASW")
tasw
# The maximum is at K=8, ASW=0.5524769

# The Average Linkage clustering at K=8 has a slightly better 
# ASW. Actually it looks much better:
mdsveronica <- cmdscale(jveronica,k=2)
# PAM with K=7:
plot(mdsveronica,pch=clusym[pclusk[[7]]$cluster],col=pclusk[[7]]$cluster)
# PAM with K=8:
plot(mdsveronica,pch=clusym[pclusk[[8]]$cluster],col=pclusk[[8]]$cluster)
# Average Linkage with K=8:
plot(mdsveronica,pch=clusym[tclusk[[8]]],col=tclusk[[8]])

plot(psil[[7]])
plot(psil[[8]])
plot(tsil[[8]])