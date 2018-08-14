#################################################################################
#####                      STAT 3019 ICA1 Part2.1                           #####
##### Introduction: Compare the cophenetic correlations of the three hiera- #####
##### rchical clusterings methods for oliveoil dataset.                     #####
#################################################################################

## (d)
data(oliveoil)
olive<- oliveoil[,3:10]
molive<- dist(olive, method = "manhattan")   # manhattan distance(L1) based on unstandardised variables as d
molive.single<-hclust(molive, method = "single") # single linkage
molive.complete<-hclust(molive, method = "complete") # complete linkage
molive.average<- hclust(molive, method = "average") # average linkage
colive.sing<-cophenetic(molive.single) # cophenetic distances of single linkage
colive.comp<-cophenetic(molive.complete) # cophenetic distances of complete linkage
colive.avrg<-cophenetic(molive.average) # cophenetic distances of average linkage

## The Cophenetic correlations c*
cor(molive,colive.sing, method = "pearson") # 0.4582669
cor(molive,colive.comp, method = "pearson") # 0.7301829 much better
cor(molive,colive.avrg, method = "pearson") # 0.7390037

## Ranking: average>complete>>single

## Plot comparing the original (manhattan) and cophenetic distances
layout(matrix(1:3, ncol = 1))
plot(colive.sing ~ molive)
abline(0,1, col = "red")
plot(colive.comp ~ molive)
abline(0,1, col = "red")
plot(colive.avrg ~ molive)
abline(0,1, col = "red")
box()
layout(1)
## (e)

molive.single9<- cutree(molive.single, 9)
adjustedRandIndex(molive.single9,oliveoil$macro.area)  # Very close to 0!! Independent random clusterings.
molive.complete9<- cutree(molive.complete, 9)
adjustedRandIndex(molive.complete9,oliveoil$macro.area) 
molive.average9<- cutree(molive.average, 9)
adjustedRandIndex(molive.average9,oliveoil$macro.area)

## Ranking: average>complete>>single
## The overall patterns are the same as the c* tells.

#################################################################################
#####                      STAT 3019 ICA1 Part2.2                           #####
##### Introduction: Run a simulation study based on data generated from the #####
##### probability model and do the cluster analysis as required.            #####
#################################################################################
library(cluster)
library(mclust)
##### (a)
##### artificial dataset function and list
art2 <- function(){
  x1 <- rnorm(20)
  y1 <- rnorm(20)
  x2 <- rnorm(20,mean=10)
  y2 <- rnorm(20)
  x3 <- runif(100,-20,30)
  y3 <- runif(100,20,40)
  clusterdata2 <- cbind(c(x1,x2,x3),c(y1,y2,y3))
  cvec <- c(rep(1,20),rep(2,20),rep(3,100))
  out <- list(data=clusterdata2,cvec=cvec)
  out
}


dat<-list() # created a list for the 20 datasets
for (i in 1:20)
  dat[[i]] <- art2()
mandat<- list() # prepare the Manhattan distance based on standardised variables
for (i in 1:20)
  mandat[[i]] <- dist(scale(dat[[i]]$data), method = "manhattan")

## Created the empty list&vectors for looping
datk<-list()
silk<-list()
datpam<-list()
silpam<-list()
datsing<-list()
datcomp<-list()
datavg<-list()
dath.sing<-list()
dath.comp<-list()
dath.avg<-list()
silh.sing<-list()
silh.comp<-list()
silh.avg<-list()
aswk<-NA
aswpam<-NA
aswh.sing<-NA
aswh.comp<-NA
aswh.avg<-NA
maxk<-c()
maxsing<-c()
maxcomp<-c()
maxavg<-c()
maxpam<-c()
arik<-c()
ariavg<-c()
arising<-c()
aricomp<-c()
aripam<-c()

## First conducted the hierachical clusterings for the three linkages of the 20 datasets.
for(i in 1:20){
  datsing[[i]]<- hclust(mandat[[i]], method="single")
  datcomp[[i]]<- hclust(mandat[[i]], method="complete")
  datavg[[i]]<- hclust(mandat[[i]], method="average")
}

for (i in 1:20){
  
  for(k in 2:10){
    # Kmeans for dataset number i
    aswk[1]<- -1  #avoid k=1
    datk[[k]]<-kmeans(dat[[i]]$data, centers=k) # kmeans
    silk[[k]]<-silhouette(datk[[k]]$cluster, dist=mandat[[i]]) # kmeans silhouette
    aswk[k]<-summary(silk[[k]])$avg.width # asw
    # average linkage for dataset number i
    aswh.avg[1]<- -1
    dath.avg[[k]]<- cutree(datavg[[i]], k)
    silh.avg[[k]]<- silhouette(dath.avg[[k]], dist=mandat[[i]])
    aswh.avg[k]<-summary(silh.avg[[k]])$avg.width
    # single linkage for dataset number i
    aswh.sing[1]<- -1
    dath.sing[[k]]<- cutree(datsing[[i]], k)
    silh.sing[[k]]<- silhouette(dath.sing[[k]], dist=mandat[[i]])
    aswh.sing[k]<-summary(silh.sing[[k]])$avg.width
    # complete linkage for dataset number i
    aswh.comp[1]<- -1
    dath.comp[[k]]<- cutree(datcomp[[i]], k)
    silh.comp[[k]]<- silhouette(dath.comp[[k]], dist=mandat[[i]])
    aswh.comp[k]<-summary(silh.comp[[k]])$avg.width  
    # PAM for dataset number i
    aswpam[1]<- -1
    datpam[[k]]<- pam(mandat[[i]],k)
    silpam[[k]]<- silhouette(datpam[[k]], dist=mandat[[i]])
    aswpam[k]<- summary(silpam[[k]])$avg.width
  }
  maxk[i]<-which.max(aswk) # the maximum asw for the k clusters using kmeans
  arik[i]<- adjustedRandIndex(datk[[maxk[i]]]$cluster,
                              kmeans(dat[[i]]$data, centers=3)$cluster)
  maxavg[i]<-which.max(aswh.avg)# the maximum asw for the k clusters using average linkage
  ariavg[i]<- adjustedRandIndex(dath.avg[[maxavg[i]]], cutree(datavg[[i]], 3))
  maxsing[i]<-which.max(aswh.sing)# the maximum asw for the k clusters using single linkage
  arising[i]<- adjustedRandIndex(dath.sing[[maxsing[i]]], cutree(datsing[[i]], 3))
  maxcomp[i]<-which.max(aswh.comp)# the maximum asw for the k clusters using complete linkage
  aricomp[i]<- adjustedRandIndex(dath.comp[[maxcomp[i]]], cutree(datcomp[[i]], 3))
  maxpam[i]<- which.max(aswpam)# the maximum asw for the k clusters using pam
  aripam[i]<- adjustedRandIndex(datpam[[maxpam[i]]]$clustering, pam(mandat[[i]], 3)$cluster)
}


maxk
sum(maxk==3)
maxavg
sum(maxavg==3)
maxsing
sum(maxsing==3)
maxcomp
sum(maxcomp==3)
maxpam
sum(maxpam==3)
arik
mean(arik)
ariavg
mean(ariavg)
arising
mean(arising)
aricomp
mean(aricomp)
aripam
mean(aripam)

data.frame(rbind(maxk,maxavg,maxsing,maxcomp,maxpam))
data.frame(rbind(arik,ariavg,arising,aricomp,aripam))

par(mfrow=c(3,2))
plot(aswk, type='l')
plot(aswh.sing, type='l')
plot(aswh.avg, type='l')
plot(aswh.comp, type='l')
plot(aswpam, type='l')

plot(dat[[20]]$data, col=dat[[20]]$cvec, pch=clusym[dat[[20]]$cvec])
plot(dat[[20]]$data, col=datk[[2]]$cluster, pch=clusym[datk[[2]]$cluster], main="Kmeans")
plot(dat[[20]]$data, col=dath.sing[[2]], pch=clusym[dath.sing[[2]]], main="Single Linkage")
plot(dat[[20]]$data, col=dath.comp[[3]], pch=clusym[dath.comp[[3]]], main="Complete Linkage")
plot(dat[[20]]$data, col=dath.avg[[3]], pch=clusym[dath.avg[[3]]], main="Average Linkage")
plot(dat[[20]]$data, col=datpam[[3]]$cluster, pch=clusym[datpam[[3]]$cluster], main="PAM")
plot(dat[[20]]$data, col=datm[[20]]$classification, pch=clusym[datm[[20]]$classification], main="Mixture Model")

par(mfrow=c(1,1))
##### (b) Gaussian Mixtures

datm<-list()
for (i in 1:20){
  cat("\n","The number of dataset:",i,"\n")
  datm[[i]]<-Mclust(dat[[i]]$data, G=2:10)
  print(summary(datm[[i]]))
  print(summary(datm[[i]]$BIC))
  cat("The ARI comparing the model and true clustering:")
  print(adjustedRandIndex(datm[[i]]$classification, Mclust(dat[[i]]$data, G=3)$classification))
  }


datm[[20]]$classification
datm[[20]]$modelName
datm[[20]]$BIC


plot(dat[[20]]$data, col=dat[[20]]$cvec)
plot(dat[[20]]$data, col=datm[[20]]$classification)

