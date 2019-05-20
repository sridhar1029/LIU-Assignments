library(ggplot2)
library(MASS)
library(fastICA)
# #library(mcmcplots)
# library(mcmc)
# library(coda)
library(readxl)
library(e1071)
library(tree)



#Assignment 2
##1
cds = read_xls("creditscoring.xls")
colSums(is.na(cds))
cds$purpose[which(is.na(cds$purpose))] = 0
cds$good_bad = as.factor(cds$good_bad)
#split data into train test
n=dim(cds)[1]
set.seed(12345) 
id=sample(1:n, floor(n*0.5)) 
train=cds[id,] 
id1=setdiff(1:n, id)
set.seed(12345) 
id2=sample(id1, floor(n*0.25)) 
valid=cds[id2,]
id3=setdiff(id1,id2)
test=cds[id3,] 

##2
##tree deviance
print("Deviance Impurity Measure - ")
print("Performance on Train -")
dtm2_dev = tree(good_bad~., train, split = "deviance")
p1 = predict(dtm2_dev, train, type = "class")
cat("Misclassification Rate : ", sum(p1==train$good_bad)/(nrow(train)), "\n")
table(train$good_bad, p1)
print("Performance on Test -")
p2 = predict(dtm2_dev, test, type = "class")
cat("Misclassification Rate : ", sum(p2==test$good_bad)/(nrow(test)), "\n")
table(test$good_bad, p2)

##tree gini
print("Gini Impurity Measure - ")
print("Performance on Train -")
dtm2_gin = tree(good_bad~., train, split = "gini")
p1 = predict(dtm2_gin, train, type = "class")
cat("Misclassification Rate : ", sum(p1==train$good_bad)/(nrow(train)), "\n")
table(train$good_bad, p1)
print("Performance on test -")
p2 = predict(dtm2_gin, valid, type = "class")
cat("Misclassification Rate : ", sum(p2==test$good_bad)/(nrow(test)), "\n")
table(test$good_bad, p2)


##3
##depth calc
pt_train <- prune.tree(dtm2_dev)
pt_valid <- prune.tree(dtm2_dev,newdata = valid)

{plot(pt_train$size, pt_train$dev, type="b", col="green",ylim=c(260,630),
      xlab="Leaf Nodes",ylab="Deviances")
  points(x=pt_valid$size,y=pt_valid$dev, type="b", col="red")
  title(main="Training vs Validation deviances on pruned trees")
  legend("topright",legend=c("Train Data Set", "Validation Data Set"),
         col=c("green","red"),lty=1:2, cex=0.8,title="Deviances")}

#Selecting the number of leaf node from the valid set for which the deviance error is least.
best_lf <-  pt_valid$size[which.min(pt_valid$dev)]

#Optinmal Tree based on leat deviance
pt_opt  <- prune.tree(dtm2_dev,best=best_lf)

nodes_train_opt <- as.numeric(rownames(pt_opt$frame))  #getnodes
depth_train <-  max(tree:::tree.depth(nodes_train_opt)) #get depth

{plot(pt_opt)
  text(pruned_tree_opt,pretty=0)
  title(main="Optimal Tree for Training Data")}

cat("Depth of Optimal Tree for training data:",depth_train,"\n")
cat("Number of Leaf Nodes in Optimal Tree for training data:",best_lf,"\n")

cat("\nLabels used by training data:\n")
tree:::labels.tree(pt_opt)


#4
model = naiveBayes(good_bad~., train)
#train
print("Naive Bayes Model - ")
print("Performance on Train -")
pred = predict(model, train[,-ncol(train)])
cat("Misclassification Rate : ", sum(pred==train$good_bad)/(nrow(train)), "\n")
table(train$good_bad, pred)
#test
print("Performance on Test -")
pred = predict(model, test[,-ncol(test)])
cat("Misclassification Rate : ", sum(pred==test$good_bad)/(nrow(test)), "\n")
table(test$good_bad, pred)

#5
#Naive Bayes
pies = seq(0.05, 0.95, 0.05)
results_nb = matrix(nrow = 0, ncol = 3)
results_ot = matrix(nrow = 0, ncol = 3)
for(pi in pies){
  pred = as.data.frame(predict(model, test[, -ncol(test)], type = "raw"))
  pred$res = ifelse(pred$good>pi, "good", "bad")
  misCl = sum(pred$res == test$good_bad)/(nrow(pred))
  m = (test$good_bad == "good")*1
  n = (pred$res == "good")*1
  tp = sum(m*n)
  fp = abs(sum(n)-tp)/sum(abs(m-1))
  tp = tp/(sum(m))
  results_nb = rbind(results_nb, c(misCl, tp, fp))
  
  pred = as.data.frame(predict(pt_opt, test[, -ncol(test)]))
  pred$res = ifelse(pred$good>pi, "good", "bad")
  misCl = sum(pred$res == test$good_bad)/(nrow(pred))
  m = (test$good_bad == "good")*1
  n = (pred$res == "good")*1
  tp = sum(m*n)
  fp = (abs(sum(n)-tp))/sum(abs(m-1))
  tp = tp/(sum(m))
  results_ot = rbind(results_ot, c(misCl, tp, fp))
}

results_nb = as.data.frame(results_nb)
colnames(results_nb) = c("MiscRate", "TP", "FP")
results_ot = as.data.frame(results_ot)
colnames(results_ot) = c("MiscRate", "TP", "FP")

ggplot() + geom_line(data=results_nb,aes(x=FP,y=TP,color="red")) +
  geom_line(data=results_ot,aes(x=FP,y=TP,color="blue"))+ scale_color_discrete(name="Model",labels=c("Naive Bayes","Optimal Tree")) +
  geom_abline(intercept=0,slope=1)+
  xlab("FPR")+ylab("TPR")+ggtitle("ROC curve between Naive Bayes and Optimal Tree")



#6
model = naiveBayes(good_bad~., train)
#train
print("Naive Bayes Model - ")
print("Performance on Train -")
pred = as.data.frame(predict(model, train[,-ncol(train)], type = "raw"))
pred$res = ifelse((pred$good)>(pred$bad)*10, "good", "bad")
cat("Misclassification Rate : ", sum(pred$res==train$good_bad)/(nrow(train)), "\n")
table(train$good_bad, pred$res)
#test
print("Performance on Test -")
pred = as.data.frame(predict(model, test[,-ncol(test)], type = "raw"))
pred$res = ifelse((pred$good)>(pred$bad)*10, "good", "bad")
cat("Misclassification Rate : ", sum(pred$res==test$good_bad)/(nrow(test)), "\n")
table(test$good_bad, pred$res)





#Assignment3

##1
state_data = read.csv("State.csv", header = TRUE, sep = ';', dec = ',')
head(state_data)
attach(state_data)
new_data = state_data[order(MET),]
detach(state_data)

ggplot(new_data, aes(MET, EX)) + geom_point()


##2
data = new_data[,c('MET','EX')]
fit_tree <- tree(EX ~., data, control = tree.control(nrow(data), minsize = 8))
fit_tree
summary(fit_tree)
plot(fit_tree)
text(fit_tree)

cv_tree = cv.tree(fit_tree, FUN = prune.tree)
plot(cv_tree$size, cv_tree$dev)


##3

##4

##5





#Assignment 4
##1
NIR_data = read.csv("NIRSpectra.csv", header = TRUE, sep = ';', dec = ',')
head(NIR_data)
train = NIR_data[,-ncol(NIR_data)]
train_true = NIR_data[,ncol(NIR_data)]
colnames(train)
NIR_pca = prcomp(train, center = TRUE, scale. = FALSE)
a = summary(NIR_pca)
df<- t(as.data.frame(a$importance))

q = sum(NIR_pca$sdev^2)
q1 = (NIR_pca$sdev^2/q) * 100

plot(q1, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")
plot(cumsum(q1), xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

p = 0.9978
selected_pca = NIR_pca$x[,1:nrow(df[which(df[,"Cumulative Proportion"]<p),])]
plot(NIR_pca$x[,1], NIR_pca$x[,2])

#2
plot(NIR_pca$rotation[,1])
plot(NIR_pca$rotation[,2])


#3
set.seed(12345)
a <- fastICA(train, 2, alg.typ = "parallel", fun = "logcosh", alpha = 1,
             method = "C", row.norm = FALSE, maxit = 200,
             tol = 0.0001, verbose = TRUE)
#par(mfrow = c(1, 3))
plot(a$X, main = "Pre-processed data")
plot(a$X %*% a$K, main = "PCA components")

w_prime=a$K %*% a$W

plot(w_prime[,1])
plot(w_prime[,2])

plot(a$S, main = "ICA components")


# 
# ##Assignment 4 Again
# pc = princomp(train, cor = TRUE, scores = TRUE)
# plot(pc, type = "l")
# req_comp = pc$scores[,c(1,2)]
# plot(req_comp)