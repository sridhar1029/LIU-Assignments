library(ggplot2)
dataset = read.table("T1-9.dat")

mean = colMeans(dataset[, 2:8])
Sx = cov(dataset[, 2:8])
#D2M <- mahalanobis(dataset[, 2:8], mean, Sx)
m_dataset = as.matrix(dataset[2:8])
mean_matrix = matrix(rep(mean, nrow(m_dataset)),nrow=nrow(m_dataset),byrow=TRUE)
sdiff = m_dataset - mean_matrix
cal_dist = diag(sdiff %*% solve(Sx) %*% t(sdiff))
dataset$mahalanobisDist = cal_dist
orddataset = order(dataset$mahalanobisDist, decreasing = TRUE)

#setting threshold to find the outliers
threshold = 0.003
p_value = dchisq(dataset$mahalanobisDist, df = 7)
dataset$pValue = round(p_value, 4)
check_outlier = p_value < threshold
dataset$isOutlier = check_outlier

#outliers before doing p-adjust
setNames(dataset$isOutlier,dataset[,1])

ggplot(dataset,aes(y = p_value[orddataset],x = dataset[orddataset,'mahalanobisDist']))+geom_point()+geom_line(aes(y=threshold,x=dataset[orddataset,'mahalanobisDist']))+ylab("p-values")+xlab("mahalanobis distance")

# adjusting p values to scale them 
# by comparing before and after p-adjusted outliers, we can see that before we had COK,PNG,KORN and SAM as outkiers 
# but after p-adjust we have reduced the number of outliers e.g. now we have only SAM as outlier.
adj_p_value = p.adjust(p_value, method = 'bonferroni')
dataset$adj_p_value = adj_p_value
find_outliers = adj_p_value < threshold
dataset$adjusted_oultiers = find_outliers
setNames(dataset$adjusted_oultiers,dataset[,1])


ggplot(dataset,aes(y = adj_p_value[orddataset],x = dataset[orddataset,'mahalanobisDist']))+geom_point()+geom_line(aes(y=threshold,dataset[orddataset,'mahalanobisDist']))+ylab("p-values")+xlab("mahalanobis distance")

# 1.b North Korea Outlier
diff_conytry = sdiff[31,]
mahalanobisDist_country = (diff_conytry%*%solve(Sx))*(diff_conytry)
mahalanobisDist_country

euclid_dist = diff_conytry*diff_conytry
euclid_dist


