kmeans(clusterdata1, centers = 3, nstart = 1)
kmeans(clusterdata1,centers=3,nstart=100)

data(oliveoil)
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

k3<-kmeans(olive, centers = 3, nstart = 100)
plot(olive, col=k3$cluster,pch=clusym[k3$cluster])

### After scale
solive<- scale(olive)
pairs(solive, cex=0.3)
sk3<- kmeans(solive, 3, 100)
plot(solive, col=sk3$cluster,pch=clusym[sk3$cluster])

table(sk3$cluster, oliveoil$macro.area)

sk9 <- kmeans(solive, 9, 100)
plot(solive, col=sk9$cluster,pch=clusym[sk9$cluster])
table(sk9$cluster, oliveoil$macro.area)
