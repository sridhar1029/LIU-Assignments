## 732A97 Multivariate Statistical Methods
## Krzysztof Bartoszek, STIMA, IDA, LiU
## Code for Exercise session 1 2017-11- in room

## Ex 1.17/43
## data mentioned in book cannot be directly found on the publisher's www
## download alternative data:
## mvadata.zip, MVAexercise R library (problem with installing, you need to create your own NAMESPACE file)
## from https://www.karlin.mff.cuni.cz/~hlavka/stat.html

athleticrecordsHH<-read.table("athleleticrecord.dat",row.names=1)
## column names are for some reason not in the dat file
colnames(athleticrecordsHH)<-c("100m (s)","200m (s)","400m (s)","800m (s)","1500m (min)","5000m (min)","10000m (min)","Marathon (min)")
apply(athleticrecordsHH,2,mean)
cov(athleticrecordsHH)
cor(athleticrecordsHH)
heatmap(cor(athleticrecordsHH),Rowv=NA,Colv=NA)

athleticrecordsJW<-read.table("T1-9.dat",row.names=1)
## column names are for some reason not in the dat file
colnames(athleticrecordsJW)<-c("100m (s)","200m (s)","400m (s)","800m (s)","1500m (min)","3000m (min)","Marathon (min)")
apply(athleticrecordsJW,2,mean)
cov(athleticrecordsJW)
cor(athleticrecordsJW)
heatmap(cor(athleticrecordsJW),Rowv=NA,Colv=NA)
# ==========================================================================

# Ex 2.5/82
X1<-cbind(c(9,5,1),c(1,3,2))
det(cov(X1))
X2<-cbind(c(3,6,3),c(4,-2,1))
det(cov2(X))

# Ex 2.6/82
# b)
X<-rbind(c(-1,3,-2),c(2,4,2),c(5,2,3))
cov(X)
det(cov(X))

# Ex 2.9/83
X<-cbind(c(12,18,14,20,16),c(17,20,16,18,19),c(29,38,30,38,35))    
apply(X,2,mean)
v1xbar<-matrix(1,nrow=5,ncol=1)%*%matrix(vmean,ncol=3)
X-v1xbar
S<-cov(X)
det(S)
S%*%c(1,1,-1)
X[,1]+X[,2]-X[,3]

