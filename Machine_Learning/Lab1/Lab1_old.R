library(readxl)
library(MASS)
library(ggplot2)
#Assignment 1
#1
spam_data = read_xlsx("spambase.xlsx", sheet = "spambase_data")
head(spam_data)
X = spam_data[, 1:(ncol(spam_data)-1)]
Y = spam_data[, ncol(spam_data)]

## 50% of the sample size
smp_size <- floor(0.50 * nrow(spam_data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(spam_data)), size = smp_size)

train <- spam_data[train_ind, ]
X_test <- spam_data[-train_ind, 1:(ncol(spam_data)-1)]
Y_test <- spam_data[-train_ind, ncol(spam_data)]
Y_train <- Y[train_ind,]
Y_test <- Y[-train_ind,]



#2
model <- glm(Spam ~.,family=binomial(link='logit'),data=train)
summary(model)

fitted.results <- predict(model,newdata=X_test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != Y_test)
print(paste('Accuracy',1-misClasificError))

pos = which(Y_test == 1)
tn = sum(fitted.results[pos])
fn = length(pos) - tn
neg = which(Y_test == 0)
fp = sum(fitted.results[neg])
tp = length(neg) - fp
tbl1 = data.frame('not_spam'= c(tp, fn), 'spam'=c(fp, tn), row.names = c('not_spam', 'spam'))
print(tbl1)

#3
fitted.results <- predict(model,newdata=X_test,type='response')
fitted.results <- ifelse(fitted.results > 0.9,1,0)
misClasificError <- mean(fitted.results != Y_test)
print(paste('Accuracy',1-misClasificError))

pos = which(Y_test == 1)
tn = sum(fitted.results[pos])
fn = length(pos) - tn
neg = which(Y_test == 0)
fp = sum(fitted.results[neg])
tp = length(neg) - fp
tbl2 = data.frame('not_spam'= c(tp, fn), 'spam'=c(fp, tn), row.names = c('not_spam', 'spam'))
print(tbl2)

#4
library(kknn)
pred_kknn = kknn(Spam~., train, X_test, k= 30, kernel = "optimal")
fitted.results = fitted(pred_kknn)
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != Y_test)
print(paste('Accuracy',1-misClasificError))

pos = which(Y_test == 1)
tn = sum(fitted.results[pos])
fn = length(pos) - tn
neg = which(Y_test == 0)
fp = sum(fitted.results[neg])
tp = length(neg) - fp
tbl3 = data.frame('not_spam'= c(tp, fn), 'spam'=c(fp, tn), row.names = c('not_spam', 'spam'))
print(tbl3)


#5
pred_kknn = kknn(Spam~., train, X_test, k= 1, kernel = "optimal")
fitted.results = fitted(pred_kknn)
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != Y_test)
print(paste('Accuracy',1-misClasificError))

pos = which(Y_test == 1)
tn = sum(fitted.results[pos])
fn = length(pos) - tn
neg = which(Y_test == 0)
fp = sum(fitted.results[neg])
tp = length(neg) - fp
tbl4 = data.frame('not_spam'= c(tp, fn), 'spam'=c(fp, tn), row.names = c('not_spam', 'spam'))
print(tbl4)

#Assignment 3
#1
my_lm = function(X, Y, X_cv, Y_cv){
  X = as.matrix(X)
  Y = as.matrix(Y)
  X_cv = as.matrix(X_cv)
  Y_cv = as.matrix(Y_cv)
  n = dim(X_cv)[1]
  W = ginv(t(X)%*%X)%*%t(X)%*%Y
  pred_Y = X_cv%*%W
  dif = Y_cv - pred_Y
  loss_cv = sum(dif^2)/n
  print(paste("CV loss :", loss_cv))
  
  pred_Y = X%*%W
  dif = Y - pred_Y
  n = nrow(X)
  print(n)
  loss_tr = sum(dif^2)/n
  print(paste("Train loss :", loss_tr))
  return(loss_cv)
}

bsSel = function(X, Y, folds){
  set.seed(12345)
  min_loss = -1
  min_comb = c()
  for(i in 2:5){
    combs = combn(1:5, i)
    for(i in 1:ncol(combs)){
      c = combs[, i]
      print(c)
      X_sub <- X[, c]
      
      #calculating the K folds
      n = nrow(X)
      fs = as.integer(n/folds)
      rnd_ind <- sample(seq_len(n), size = n)
      cvl = 0
      for(i in 1:folds){
        if(i==folds){
          l = length(rnd_ind)
          cv_ind = rnd_ind[((fs*(i-1))+1):l]
        }
        else{
          cv_ind = rnd_ind[((fs*(i-1))+1):(fs*i)]
        }
        X_train = X_sub[-cv_ind,]
        Y_train = Y[-cv_ind]
        X_cv = X_sub[cv_ind,]
        Y_cv = Y[cv_ind]
        cv_loss = my_lm(X_train, Y_train, X_cv, Y_cv)
        cvl = cvl + cv_loss
      }
      loss = cvl/folds
      #end of calc
      
      if(loss<min_loss){
        min_loss = loss
        min_comb = c
      }else if(min_loss == -1){
        min_loss = loss
        min_comb = c
      }
    }
  }
  return(c(min_loss, min_comb))
}

data = swiss
head(data)
Y = data$Fertility
X = data[,2:ncol(data)]

best = bsSel(X,Y, 5)
print(best)


# folds = 5
# n = nrow(data)
# fs = as.integer(n/folds)
# rnd_ind <- sample(seq_len(n), size = n)
# for(i in 1:folds){
#   if(i==folds){
#     l = length(rnd_ind)
#     cv_ind = rnd_ind[((fs*(i-1))+1):l]
#   }
#   else{
#     cv_ind = rnd_ind[((fs*(i-1))+1):(fs*i)]
#   }
#   print(data[-cv_ind,])
# }


# Assignment 4
#1
tecator_data = read_xlsx("tecator.xlsx", sheet = "data")
head(tecator_data)
data = tecator_data[, c("Protein", "Moisture")]
data$Intercept = 1
head(data)

## 50% of the sample size
smp_size <- floor(0.50 * nrow(data))

## set the seed to make your partition reproducible
set.seed(12345)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
X_train = data[train_ind, c("Protein", "Intercept")]
Y_train <- data[train_ind, "Moisture"]
X_test <- data[-train_ind, c("Protein", "Intercept")]
Y_test <- data[-train_ind, "Moisture"]

ggplot(tecator_data, aes(Protein, Moisture)) + geom_point()

#2
train2 = train
X_test2 = X_test
losses = matrix(0, nrow = 0, ncol = 2)
for(i in 1:6){
  if(i>1){
    train2[paste('P', i)] = sapply(train2['Protein'], function(x) x^i)
    X_test2[paste('P', i)] = sapply(X_test2['Protein'], function(x) x^i)
  }

  model <- glm(Moisture ~.,family=gaussian(link = "identity"),data=train2)
  summary(model)

  fitted.results <- predict(model,newdata=X_test2,type='response')
  diff = fitted.results - Y_test
  cost_test = sum(diff*diff)/nrow(Y_test)
  print(paste('CV Cost',cost_test))

  x_tr = train2[ , !(names(train2) %in% "Moisture")]
  y_tr = train2['Moisture']
   fitted.results <- predict(model,newdata=x_tr,type='response')
  diff = fitted.results - y_tr
  cost_train = sum(diff*diff)/nrow(y_tr)
  print(paste('Train Cost',cost_train))
  print("")
  losses = rbind(losses, c(cost_train, cost_test))
}



# my_lm2 = function(X, Y){
#   X = as.matrix(X)
#   Y = as.matrix(Y)
#   W = ginv(t(X)%*%X)%*%t(X)%*%Y
#   return(W)
# }
# 
# 
# 
# train2 = train
# X_test2 = X_test
# losses = matrix(0, nrow = 0, ncol = 2)
# for(i in 1:6){
#   if(i>1){
#     train2[paste('P', i)] = sapply(train2['Protein'], function(x) x^i)
#     X_test2[paste('P', i)] = sapply(X_test2['Protein'], function(x) x^i)
#   }
#   x_tr = train2[ , !(names(train2) %in% "Moisture")]
#   y_tr = train2['Moisture']
#   
#   W = my_lm2(x_tr, y_tr)
#   print(W)
#   pred_Y = as.matrix(X_test2)%*%W
#   dif = Y_test - pred_Y
#   loss_cv = sum(dif^2)/nrow(X_test2)
#   print(paste("CV loss :", loss_cv))
#   
#   pred_Y = as.matrix(x_tr)%*%W
#   dif = y_tr - pred_Y
#   loss_tr = sum(dif^2)/nrow(x_tr)
#   print(paste("Train loss :", loss_tr))
#   #print(paste('CV Cost',cost))
#   print("")
#   losses = rbind(losses, c(loss_tr, loss_cv))
# }
dim(train)
dim(X_test)
losses
los = data.frame(losses)
colnames(los)=c("Train_Loss", "Test_Loss")
los$Param = rownames(los)
los

ggplot(los) + geom_point(aes(Param, Train_Loss), col="red") + geom_point(aes(Param, Test_Loss), col="blue")




#4
colnames(tecator_data)
head(tecator_data)
data = tecator_data
data$Moisture = NULL
data$Sample = NULL
data$Protein = NULL

colnames(data)

model <- glm(Fat ~.,family=gaussian(link = "identity"),data=data)
summary(model)

step = stepAIC(model, trace = F)
step$anova
model.weights
