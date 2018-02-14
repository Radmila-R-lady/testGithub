
library(foreign)
ESSCH<-as.data.frame(read.spss("D:/2 nd semester/unilu/cluster and factor analysis/as 3/ESSCH.sav", use.value.labels=T, max.value.labels=Inf, use.missings=F))
attach(ESSCH)

# 1. prepare your data for the analysis

# recoding: transformation into metrical variables
table(tstrprh)
tstrprh[tstrprh=="Don't know"]<-NA
tstrprh<-tstrprh[drop=T]
plumber<-as.numeric(tstrprh)
table(tstrprh)
detach(ESSCH)
ESSCH$plumber<-plumber
rm("plumber", "tstrprh")
attach(ESSCH)

table(tstfnch)
summary(tstfnch)
tstfnch[tstfnch=="Don't know"]<-NA
tstfnch<-tstfnch[drop=T]
financial<-as.numeric(tstfnch)
table(tstfnch)
summary(tstfnch)
detach(ESSCH)
ESSCH$financial<-financial
rm("financial", "tstfnch")
attach(ESSCH)

table(tstfnch)
tstfnch[tstfnch=="Don't know"]<-NA
tstfnch<-tstfnch[drop=T]
financial<-as.numeric(tstfnch)
table(tstfnch)
detach(ESSCH)
ESSCH$financial<-financial
rm("financial", "tstfnch")
attach(ESSCH)

table(tstpboh)
tstpboh[tstpboh=="Don't know"]<-NA
tstpboh<-tstpboh[drop=T]
public<-as.numeric(tstpboh)
table(tstpboh)
detach(ESSCH)
ESSCH$public<-public
rm("public", "tstpboh")
attach(ESSCH)


# additional variables (recoding without attach..)
table(ESSCH$wrytrdh)

#deleting don't know's 
ESSCH$wrytrdh[ESSCH$wrytrdh=="Don't know"]<-NA
ESSCH$wrytrdh<-ESSCH$wrytrdh[drop=T]
table(ESSCH$wrytrdh)
attach(ESSCH)
# defining one object containing the variables 
attach(ESSCH)
cluster.set<-cbind(idno, plumber, financial, public) 
detach(ESSCH)


#dealing with missings

sum(is.na(cluster.set))
cluster.set<-na.omit(cluster.set)


# Stage 2
# 1. detecting outliers
# dissimilarity table (cf. Hair 543)
cluster.data<-as.data.frame(cluster.set)
attach(cluster.data)


#differences from the mean for each observation, mean is the average respondent

plumber.diss<-plumber-mean(plumber)
#squared differences from the mean
plumber.sq<-plumber.diss^2
financial.diss<-financial-mean(financial)
financial.sq<-financial.diss^2
public.diss<-public-mean(public)
public.sq<-public.diss^2
detach(cluster.data)

# Dissimilarities
dissimis<-cbind(plumber.diss, financial.diss, public.diss)
# squared Dissimilarities
sq.dissimis<-cbind(plumber.sq, financial.sq, public.sq)
# Average Dissimilarity (root of sum of sq dissimis)
#in order to create an estimate of the dissimilarity 
diss.value<-sqrt(plumber.sq+financial.sq+public.sq)
mean(diss.value)
summary(diss.value)
# Table of Dissimilarities
diss.table<-cbind(cluster.data$idno, dissimis, sq.dissimis, diss.value)

plot(diss.value)
identify(diss.value, n=10, 
         label=cluster.data$idno)

# standardisation of variables (non-obligatory in this case)
cluster.data.stand<-scale(cluster.data)


# 2. choosing a distance measure (here: euclidian) (Hair 544)
# and producing a dissimilarity structure
euc1.cluster<-dist(cluster.data, method="euclidean", diag=F, upper=F)
# alternative: daisy-function of package cluster
# install.packages("cluster")
library(cluster)
euc2.cluster<-daisy(cluster.data, 
                    metric="euclidean")
# Summary and comparing of the two distance objects
summary(euc1.cluster)
summary(euc2.cluster)
#as we see both function give the same output
# mahalanobis distance matrix (cf. Hair 522)
library(StatMatch)
maha.cluster<-mahalanobis.dist(cluster.data)
maha.cluster<-as.dist(maha.cluster)
summary(maha.cluster)

# Stage 4
# 1. hierachical clustering and dendrograms
# Ward clustering
hi.cluster1a<-hclust(euc1.cluster, 
                     method="ward.D")
hi.cluster1b<-hclust(euc2.cluster, 
                     method="ward.D2")
hi.cluster1c<-hclust(maha.cluster,                      
                     method="ward.D2")
summary(hi.cluster1a)
summary(hi.cluster1b)
summary(hi.cluster1c)

# complete linkage
hi.cluster2a<-hclust(euc1.cluster, 
                     method="complete")
hi.cluster2b<-hclust(maha.cluster, 
                     method="complete")
summary(hi.cluster2a)
summary(hi.cluster2b)

#centroid linkage
hi.cluster3a<-hclust(maha.cluster, 
                     method="centroid")



hi.cluster3a
hi.cluster3b<-hclust(euc1.cluster, 
                     method="centroid")

# Dendograms
# Ward
par(mfrow=c(1,3))
plot(hi.cluster1a)
plot(hi.cluster1b)
plot(hi.cluster1c)
par(mfrow=c(1,1))

# complete
par(mfrow=c(1,2))
plot(hi.cluster2a)
plot(hi.cluster2b)
par(mfrow=c(1,1))

#centroid
plot(hi.cluster3a)
plot(hi.cluster3b)



# mark a set number of clusters in the dendrogram, e.g. 5
plot(hi.cluster1a)
rect.hclust(hi.cluster1a, k=2, 
            border="blue")
rect.hclust(hi.cluster1a, k=3, 
            border="blue")

plot(hi.cluster2a)
rect.hclust(hi.cluster2a, k=5, 
            border="red")
rect.hclust(hi.cluster2a, k=6, 
            border="green")
rect.hclust(hi.cluster2a, k=3,
            border="blue")



# other dendrograms
hcd <- as.dendrogram(hi.cluster1a)
plot(hcd, type="triangle")

# Another very useful option is the ability to inspect selected 
# parts of a given tree. For instance, the top partitions of the dendrogram, 
# we cut it at a height of 50
par(mfrow = c(1, 2))
plot(cut(hcd, h = 50)$upper, 
     main = "Dendogram for deciding on number of clusters")
plot(cut(hcd, h =10)$lower[[1]], main = "First branch of lower tree with cut at h=50")
plot(cut(hcd, h = 50)$lower[[2]], main = "Second branch of lower tree with cut at h=50")
plot(cut(hcd, h = 50)$lower[[3]], main = "Third branch of lower tree with cut at h=50")
plot(cut(hcd, h = 50)$lower[[4]], main = "Fourth branch of lower tree with cut at h=50")
plot(cut(hcd, h = 50)$lower[[5]], main = "Fifth branch of lower tree with cut at h=50")
plot(cut(hcd, h = 50)$lower[[6]], main = "Sixth branch of lower tree with cut at h=50")
# etc.
par(mfrow=c(1,1))

# for detecting outliers, look at the algorithm schedule $merge: 
# an n-1 by 2 matrix. Row i of merge describes the merging of clusters at step i 
# of the clustering. If an element j in the row is negative, then observation -j 
# was merged at this stage. If j is positive then the merge was with the cluster 
# formed at the (earlier) stage j of the algorithm. Thus negative entries in merge 
# indicate agglomerations of singletons, and positive entries indicate agglomerations 
# of non-singletons (cf. Hair et al. p. 550)
hi.cluster1a$merge
hi.cluster1b$merge
hi.cluster2a$merge

# going back to dissimilarity table to check possible outliers
diss.table[1063,"diss.value"]
diss.table[904,"diss.value"]
diss.table[913,"diss.value"]
diss.table[580,"diss.value"]
diss.table[270,"diss.value"]
mean(diss.table[,"diss.value"])
summary(diss.table[,"diss.value"])

# alernatively use agnes for hierarchical cluster analysis
hi.cluster3<-agnes(euc1.cluster, 
                   diss=T, 
                   method="ward")
summary(hi.cluster3)
hi.cluster3$merge
# or on the data frame
hi.cluster3aa<-agnes(cluster.data, 
                    diss=F, 
                    metric="euclidean", method="ward")
hi.cluster3aa$merge
plot(hi.cluster3aa)

# producing dendrograms
plot(hi.cluster3, which.plots=2)
pltree(hi.cluster3)
rect.hclust(hi.cluster3, k=4, border="green")

# agglomeration coefficients: average similarity,  
# a higher AC indicates better quality of clustering 
# and a better fit of the dendrogram.
coefHier(hi.cluster1a)
coefHier(hi.cluster1b)
coefHier(hi.cluster1c)
coefHier(hi.cluster2a)
coefHier(hi.cluster2b)
coefHier(hi.cluster3)
coefHier(hi.cluster3a)
# for object agnes also (only for agnes function):
hi.cluster3$ac

# plot of within groups sum of squares by number 
# of clusters extracted can help to determine
# the appropriate number of clusters. 
# Look for a bend similar to a scree test. cf. Quick R-Site
wss<-(nrow(cluster.set)-1)*sum(apply(cluster.set, 2, var))
for(i in 2:6)wss[i]<-sum(kmeans(cluster.set, centers=i)$withinss)
plot(1:6, wss, type="b", xlab="number of clusters", ylab="within groups sum of squares")


#gap statistics
install.packages("lga")
library(lga)
cluster.set<-cbind(idno, plumber, financial, public) 
cluster.set<-cbind(uid,f1a_1,f1a_2)
lga
lga(cluster.set,2)
lga(cluster.set,3)
lga(cluster.set,4)

cluster.data

# 2. kmeans/non-hierarchical clustering (stages 4-6)
non.cluster<-kmeans(cluster.data[,2:4], 2)
summary(non.cluster)
non.cluster$cluster
# size of clusters
non.cluster$size
# in order to keep cluster-solution stable, set-seed-fun
set.seed(17)
non.cluster<-kmeans(cluster.data[,2:4], 4)
summary(non.cluster)
non.cluster$size
non.cluster$cluster

# pam clustering as robust alternative to kmeans
pam.cluster<-pam(cluster.data[,2:4],4)
summary(pam.cluster)
pam.cluster$medoids
pam.cluster$clustering
pam.cluster$id.med # giving the indices
pam.cluster$clusinfo


# mean values of the clustering variables for the clusters
#e.g. trust in political parties: cluster 4 lowest and cluster 3
#the highest mean  
aggregate(cluster.data, 
          by=list(non.cluster$cluster), 
          FUN=mean)

aggregate(cluster.data, by=list(pam.cluster$cluster), FUN=mean)

# compared to means over all cases if solution is 4 clusters
#e.g. mean of plumber is 3.23; I have 3 clusters above and 1 below.
#mean of financial is 2.83. 2 clusters above and one below  
#mean of public is 3.13 1 above 3 below

summary(cluster.data)


# 4. Inserting the Cluster-Variable into the ESS-Dataset
# 4a. Add cluster variable to cluster.data
data.cluster<-data.frame(cluster.data, non.cluster$cluster)
data.cluster<-data.frame(data.cluster, pam.cluster$clustering)
# 4b. Inserting the cluster variable into the original data frame
ESS.cluster<-merge(ESSCH, data.cluster[,c(1,5)], by="idno", all.x=T)
View(ESS.cluster[,606:609])
#It contains all the variables 


# comparison of the two cluster solutions
table(ESS.cluster$pam.cluster.clustering, ESS.cluster$non.cluster.cluster)



# now you can check the interrelation of the clusters 
# with sociodemographics and political orientation...
attach(ESS.cluster)
# comparing age means of clusters
tapply(agea, non.cluster.cluster, mean, na.rm=T)
install.packages("vcd")
library(vcd)
assocstats(table(agea, non.cluster.cluster))
# gender distribution among clusters
round(100*prop.table(table(gndr, non.cluster.cluster),2),1)
# party preference and clusters..
round(100*prop.table(table(wrytrdh, non.cluster.cluster),2),1)
round(100*prop.table(table(non.cluster.cluster,wrytrdh ),2),1)



fit<-kmeans(ESS.cluster, 3)
fit
library(cluster) 
clusplot(ESS.cluster, fit$cluster, color=TRUE, shade=TRUE, 
  	labels=2, lines=0)
install.packages("fpc")
library(fpc)
plotcluster(ESS.cluster, fit$cluster)
# etc....

# clean up and close!
rm(list=ls())




