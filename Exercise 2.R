####### Question 2

set.seed(123456)
cgolive1 <- clusGap(olive,kmeans,K.max = 25, B=100, d.power=2,spaceH0="original",nstart=100)
print(cgolive1,method="Tibs2001SEmax")
plot(cgolive1)


## Try scaled version
set.seed(123456)
cgolive2 <- clusGap(solive, kmeans, K.max = 20, B=100, d.power =2, spaceH0 = "original", nstart=100)
print(cgolive2,method="Tibs2001SEmax")
plot(cgolive2)


### aritificial dataset 2

set.seed(123456)
cgart1 <- clusGap(clusterdata2, kmeans, K.max = 20, B=100, d.power = 2, spaceH0 = "original", nstart=100)
print(cgart1,method="Tibs2001SEmax")
plot(cgart1)

cgart2 <- clusGap(clusterdata2, kmeans, K.max = 20, B=100, d.power = 2, spaceH0 = "original", nstart=1)
print(cgart2, method="Tibs2001SEmax")
plot(cgart2)





######## Question 3
set.seed(1234567)
cgart3 <- clusGap(clusterdata2, kmeans, K.max = 20, B=100, d.power = 2,spaceH0 = "scaledPCA",nstart=100)
print(cgart3,method="Tibs2001SEmax")
plot(cgart3)


print(cgart1, method= "firstSEmax")




