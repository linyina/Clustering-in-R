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

veronica <- read.table("./Data/veronica.dat.txt")
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
clusterdata1 <- as.matrix(read.table("./Data/clusterdata1.dat.txt"))
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
clusterdata2 <- as.matrix(read.table("./Data/clusterdata2.dat.txt"))
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
