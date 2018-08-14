############################################
#####  Step 2: Kmeans clustering       #####
############################################
## random try
sdortmundk5<- kmeans(sdortmund, centers = 5, nstart = 100)
pairs(sdortmund, col=sdortmundk5$cluster, pch=clusym[sdortmundk5$cluster])
sdortmundk5$centers
## the means (scaled) of each clusters in different factors


## Cluster
cg1<- clusGap(dortmund,kmeans,20,B=100,d.power=2,spaceH0="scaledPCA",nstart=100)   # 1???
plot(cg1)
print(cg1, method = "firstSEmax")
plot(1:20,cg1$Tab[,1],xlab="k",ylab="log S_k",type="l")
points(1:20,cg1$Tab[,2],xlab="k",ylab="log S_k",type="l",lty=2)
legend("topright",c("log S_k in data","E(log S_k) uniform"),lty=1:2) 
dev.copy(pdf,"clusgap logsk plots.pdf")
dev.off()
help(legend)
#### scale
sdortmund<- scale(dortmund)
pairs(dortmund)
cg2<-clusGap(sdortmund, kmeans, K.max = 20, B=100, d.power = 2, iter.max=20,spaceH0 = "original", nstart=100)
plot(cg2)
dev.copy(pdf, "clusgap_scaled plots.pdf")
dev.off()
print(cg2, method = "Tibs2001SEmax")
print(cg2, method = "firstSEmax")
plot(1:20,cg2$Tab[,1],xlab="k",ylab="log S_k",type="l", ylim=c(6,10))
points(1:20,cg2$Tab[,2],xlab="k",ylab="log S_k",type="l",lty=2)
legend("topright",c("log S_k in data","E(log S_k) uniform"),lty=1:2) 
dev.copy(pdf, "clusgap_logSKplots_original_scaled")
dev.off()

cg3<- clusGap(sdortmund, kmeans, K.max = 20, B=100, d.power = 2, iter.max=20, spaceH0 = "scaledPCA", nstart=100)
print(cg3, method="Tibs2001SEmax")
print(cg3, method = "firstSEmax")  # 3 unchanged
plot(cg3) # values of gap
dev.copy(pdf, "clusgap_scaled plots_scaledPCA.pdf")
dev.off()
plot(1:20,cg3$Tab[,1],xlab="k",ylab="log S_k",type="l", ylim=c(6,10))
points(1:20,cg3$Tab[,2],xlab="k",ylab="log S_k",type="l",lty=2)
legend("topright",c("log S_k in data","E(log S_k) uniform"),lty=1:2) 
dev.copy(pdf, "clusgap_logSKplots_scaledPCA_scaled")
dev.off()

adjustedRandIndex(kmbundestag5$cluster,wbundestag5)


####### PAM
dortmundp5 <- pam(dortmund, 5)

help(pam)



####### Distances
eusdortmund <- dist(sdortmund,method="euclidean")
mansdortmund <- dist(sdortmund,method="manhattan")
plot(eusdortmund, mansdortmund)


olivecov <- cov(olive)
molive <- as.matrix(olive)
for (i in 1:572)
  mahalm[i,] <- mahalanobis(molive,molive[i,],olivecov)

mahalm <- matrix(0,ncol=170,nrow=170)
sdortmundcov<- cov(sdortmund)
sdortmund1 <- as.matrix(sdortmund)
for (i in 1:170)
  mahalm[i,] <- mahalanobis(sdortmund1, sdortmund1[i,], sdortmundcov)
help("mahalanobis")
