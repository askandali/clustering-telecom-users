library(corrgram)
library(factoextra)
library(HDclassif)
library(cluster)
library(mclust)
library(FactMixtAnalysis)
library(nnet)
library(class)
library(tree)


cluster<-read.table("data/churn.txt",header=T,dec=",")
names(cluster)
attach(cluster)

#Set dataset
areaCode<-factor(AreaCode,c(408,415,510),c("San Jose","San Francisco","Oakland"))
churn<-factor(Churn,c(0,1),c("In","Out"))
vmailPlan<-factor(VMailPlan,c(0,1),c("No","Yes"))
intlPlan<-factor(Int.lPlan,c(0,1),c("No","Yes"))
clust<-data.frame(AccountLength,VMailMessage,DayMins,EveMins,NightMins,IntlMins,
                  CustServCalls,DayCalls,DayCharge,EveCalls,EveCharge,NightCalls,NightCharge,
                  IntlCalls,IntlCharge,Gender,areaCode,churn,vmailPlan,intlPlan)
head(clust)

#Descriptives
#Checking for correlation between variables
corrgram(clust)
names(clust)
attach(clust)

scaled<-data.frame(DayMins,EveMins,NightMins,IntlMins,VMailMessage,CustServCalls)
head(scaled)
scaled<-scale(scaled)
head(scaled)

###########

#####Clustering######
#ratio variable
daynightMins<-DayMins/NightMins
mean(daynightMins)

#data needed for clustering
clust<-data.frame(daynightMins,IntlMins)
clust<-scale(clust)
corrgram(clust)

#Calculate silhouette value
silh_hierarchical<-fviz_nbclust(clust, FUN = hcut, method = "silhouette")
silh_kmeans<-fviz_nbclust(clust, FUN = kmeans, method = "silhouette")

#Optimal number of clusters
silh_hierarchical## 2clusters
silh_kmeans###3 clusters

#Similar Silhouette values
max(silh_hierarchical$data[,2])
max(silh_kmeans$data[,2])


#### Hierarchical Clustering 
hc1<-hclust(dist(clust),method="ward.D")
summary(hc1)

clas<-cutree(hc1,2)
pairs(clust,col=clas)
fviz_cluster(list(data = clust, cluster = clas))

##WILKS
m <- manova(clust~clas)
summary(m,test="Wilks")
mywilks<- summary(m,test="Wilks")$stats[1,2]
mywilks
by(clust,clas,summary)
aggregate(clust,by=list(clas),mean)

#########  K-means
sol<-kmeans(clust,3)
pairs(clust,col=sol$cluster)
sol$center
fviz_cluster(list(data = clust, cluster = sol$cluster))
by(clust,sol$cluster,summary)
aggregate(clust,by=list(sol$cluster),mean)
aggregate(scaled,by=list(sol$cluster),mean)
##WILKS
m <- manova(clust~sol$cluster)
summary(m,test="Wilks")
mywilks<- summary(m,test="Wilks")$stats[1,2]
mywilks

#####BOXPLOTS###
par(mfrow=c(1,3))

boxplot( split(DayMins,sol$cluster), cex.axis=0.6, las=3 ,col="lightsteelblue4",main="Day minutes")
#title(main=" Minutes Per group",outer = TRUE)
boxplot( split(NightMins,sol$cluster), cex.axis=0.6, las=3 ,col="lightsteelblue4",main="Night minutes")
boxplot( split(IntlMins, sol$cluster), cex.axis=0.6, las=3 ,col="lightsteelblue4",main="International minutes")

#####BARPLOTS#########
par(mfrow=c(1,2))
barplot(t(table(Gender,sol$cluster)),col=c("lightsteelblue4","lightsteelblue3","lightsteelblue1"),main="Groups per gender",ylim=c(0,2200))
legend("topright", c("Group 1","Group 2","Group 3"), pch=15, 
       col=c("lightsteelblue4","lightsteelblue3","lightsteelblue2"), 
       bty="n")
#

barplot(t(table(areaCode,sol$cluster)),col=c("lightsteelblue4","lightsteelblue3","lightsteelblue1"),main="Groups per location",ylim=c(0,2000))
legend("topright", c("Group 1","Group 2","Group 3"), pch=15, 
       col=c("lightsteelblue4","lightsteelblue3","lightsteelblue2"), 
       bty="n")


######HORIZONTAL BARPLOTS####################
par(mfrow=c(1,1))
barplot(matrix(c(-0.16,0.008,0.25,-0.91,0.002,0.02,0.94,0.031,-1.14,-0.012,0.013,-0.01,-0.21,-0.018,0.20,0.72,-0.007,-0.011),nr=6), beside=T, 
        col=c("lightseagreen","orange2","palevioletred3","slateblue3","red4","palegreen3"), 
        names.arg=c("Group 1","Group 2","Group 3"),xlim = c(-2,2),horiz = T,main="Group Characteristics")
legend("topright", c("DayMins","EveMins","NightMins","IntlMins","VMailMessage","CustServCalls"), pch=15, 
       col=c("lightseagreen","orange2","palevioletred3","slateblue3","red4","palegreen3"), 
       bty="n")

