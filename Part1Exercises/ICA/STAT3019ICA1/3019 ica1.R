###################################################################################
#####              This is for STAT3019 ICA 1 part1 - Clustering              #####
#####                                                                         #####
###################################################################################
library(MASS)
library(flexclust)
library(fpc)
library(pdfCluster)
library(mclust)
library(cluster)
dortmund <- read.table("Dortmund_G3019ica.dat.txt",header=TRUE)
View(dortmund)
str(dortmund)
summary(dortmund)
### 170 obs. of 30 variables
attach(dortmund)

############################################
#####  Step 1: Exploratory Analysis    #####
############################################

#### 1.1 Boxplots
quartz(width=8, height = 6)
boxplot(births, deaths)
### Buildings by years
buildings.years<-data.frame(buildings_until_1900, buildings_1900.1918,buildings_1919.1948,buildings_1949.1957, 
                            buildings_1958.1962, buildings_1963.1972,buildings_1973.1982,buildings_1983.1992,
                            buildings_1993.2001)
boxplot(buildings.years, xaxt="n",xlab="Years", ylab= "Numbers of buildings",
        main="Numbers of buildings by year of construction")
axis(1,at=c(1:9), labels=c(".-1900","1900-1918", "1919-1948", "1949-1957", 
                                        "1958-1962", "1963-1972","1973-1982", "1983-1992", "1993-2001"))
points(apply(buildings.years,2,mean),col="red",pch=15) 
dev.copy(pdf,"Boxplot_buildingnum.pdf")
dev.off()
### Age
age<- data.frame(age_under_26, age_26.35, age_36.45, age_46.55, age_56.65, age_above_65)
boxplot(age, xaxt="n", xlab="Age group", ylab="Number of inhabitants", 
        main="Number of inhabitants by age group")
axis(1,at=c(1:6), labels=c("under26", "26-35", "36-45", "46-55", "56-65", "above65"))
points(apply(age,2,mean),col="red",pch=15)
dev.copy(pdf,"Boxplot_agenum.pdf")
dev.off()
### Gender
boxplot(female, male, ylab="Number of inhabitants", main="Number of inhabitants by gender", xaxt="n")
axis(1, at=c(1,2), labels= c("female", "male"))
points(c(mean(female), mean(male)), col="red", pch=15)
dev.copy(pdf,"Boxplot_gender.pdf")
dev.off()

### Correlation/Collinearity between variables
dortmund.cor<-data.frame(cor(dortmund, method = "spearman"))
dortmund.cor>=0.9
cor(age)
pairs(age) # correlations
cor(buildings.years)
pairs(buildings.years) # random
plot(moves_in, moves_out)
lines(lowess(moves_in, moves_out), col='red', lwd=2)

pairs(data.frame(households, unemployed, benefits, births, children, deaths, area_buildings, age_under_26, social_insurance))
plot(households, children)
lines(lowess(households,children), col='red', lwd=2)
plot(benefits~unemployed)
lines(lowess(benefits~unemployed),  col="red", lwd=2)


# lots of strong correlations - dependency violation?? spherical??

pairs(data.frame(area_buildings, cars, male, trucks, female, social_insurance, motorbikes))
plot(area_buildings, cars)
lines(lowess(area_buildings,cars), col='red', lwd=2)
plot(male,area_buildings)
lines(lowess(male,area_buildings), col='red', lwd=2)
plot(male, cars)
lines(lowess(male, cars), col='red', lwd=2)
plot(male,female)
lines(lowess(male,female), col='red', lwd=2)
plot(cars, motorbikes)
lines(lowess(cars,motorbikes), col='red', lwd=2)

Ref.pca(data.frame(area_buildings, cars, male, female, motorbikes, social_insurance))
Ref.pca(data.frame(households, children))  # delete children
Ref.pca(data.frame(benefits, unemployed))
Ref.pca(data.frame(male, female))
Ref.pca(data.frame(area_buildings, cars))

### PCA
Ref.pca<- function(x){
  pca.scale<- scale(x)
  test.pr <- prcomp(pca.scale)
  options(digits = 4)
  print(summary(test.pr))
  print(test.pr)
  screeplot(test.pr, type="lines")
}

Ref.pca(age)
summary(prcomp(age, scale. = TRUE))
age.pca<- prcomp(age, scale. = TRUE)$x[,1:2]
Ref.pca(buildings.years)
buildings.cg<-clusGap(t(scale(buildings.years)), kmeans,K.max = 8,B=100,d.power=2,spaceH0="original",nstart=100)  # 8... not working
print(buildings.cg, method = "Tibs2001SEmax")
print(buildings.cg, method = "firstSEmax")

Ref.pca(data.frame(moves_in,moves_out))
prcomp(data.frame(moves_in,moves_out), scale. = TRUE)

## Buildings:hclust - complete: they are similar everywhere
## we cluster the variables to reduce the dimensions
eu.buil<- dist(scale(t(buildings.years)), method = "euclidean")
buil.hc<- hclust(eu.buil, method="average")
plot(buil.hc)
## Buildings: hclust - complete: they are similar everywhere
## data.frame(buildings_1949.1957,buildings_1900.1918,buildings_1919.1948)
## data.frame(buildings_1973.1982, buildings_1983.1992, buildings_1963.1972, buildings_1993.2001)
## data.frame(buildings_until_1900, buildings_1958.1962)


## Choosing distance: choose similar values rather than similar patterns for combining districts with 
## different sizes, which may be better for monitoring. - background information


## Silhouette
## summary(silhouette(sdortmund.newk12$cluster, dist = eusdortmund.new))$avg.width

