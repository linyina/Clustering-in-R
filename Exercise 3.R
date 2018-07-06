library(cluster)
library(StatMatch)

help(daisy)
### Q4
Ex3.4 <- data.frame(c("blue",'red','red'),
                    c(1,0,1),
                    c(1,NA,0),
                    c(12,NA,17))
daisy(Ex3.4, metric = c("gower"))





#### q5
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