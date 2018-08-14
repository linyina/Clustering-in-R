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