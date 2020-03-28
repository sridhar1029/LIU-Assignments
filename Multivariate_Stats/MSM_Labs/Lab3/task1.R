
data <- read.table("T1-9.dat")

subdata = data[,-1]
countryName = data[,1]

# a
correlation_matrix = cor(subdata)

eigenlist <- eigen(correlation_matrix)

eigenValues<- eigenlist$values
eigenVector<- eigenlist$vectors

# b
prca <- prcomp(eigenVector, rank. = 2, scale. = TRUE)

summary(prca)
#print(prca)

# c
plot(prca)


#d

library(plyr)
a1 <- prca$rotation[,1]
center <- prca$center
scale <- prca$scale
hm <- as.matrix(subdata)
prca_score <- drop(scale(hm,center = center,scale = scale) %*% a1)
prca_score<- data.frame(prca_score)
prca_score<- cbind(countryName,prca_score)
colnames(prca_score) <- c("Country","Score")
arrange(prca_score,desc(Score))


