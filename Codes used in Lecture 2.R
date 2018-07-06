###### Lecture 2


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
