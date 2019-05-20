library(readxl)
library(ggplot2)
library(kknn)
library(MASS)
library(glmnet)
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

train = spam_data[train_ind, ]
X_train = X[train_ind,]
X_test = X[-train_ind,]
Y_train = Y[train_ind,]
Y_test = Y[-train_ind,]

confMat = function(pred, actual){
  pos = which(actual == 1)
  tn = sum(pred[pos])
  fn = length(pos) - tn
  neg = which(actual == 0)
  fp = sum(pred[neg])
  tp = length(neg) - fp
  tbl = data.frame('not_spam'= c(tp, fn), 'spam'=c(fp, tn), row.names = c('not_spam', 'spam'))
  return(tbl)
}

#2
model <- glm(Spam ~.,family=binomial(link='logit'),data=train)
summary(model)

##Train predict
print("Train Results -")
fitted.results <- predict(model,newdata=X_train,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != Y_train)
print(paste('Train Accuracy',1-misClasificError))
tbl1_train = confMat(fitted.results, Y_train)
print(tbl1_train)

##Test predict
print("Test Results -")
fitted.results <- predict(model,newdata=X_test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != Y_test)
print(paste('Test Accuracy',1-misClasificError))
tbl1_test = confMat(fitted.results, Y_test)
print(tbl1_test)

#3

##Train predict
print("Train Results -")
fitted.results <- predict(model,newdata=X_train,type='response')
fitted.results <- ifelse(fitted.results > 0.9,1,0)
misClasificError <- mean(fitted.results != Y_train)
print(paste('Train Accuracy',1-misClasificError))
tbl2_train = confMat(fitted.results, Y_train)
print(tbl2_train)

##Test predict
print("Test Results -")
fitted.results <- predict(model,newdata=X_test,type='response')
fitted.results <- ifelse(fitted.results > 0.9,1,0)
misClasificError <- mean(fitted.results != Y_test)
print(paste('Test Accuracy',1-misClasificError))
tbl2_test = confMat(fitted.results, Y_test)
print(tbl2_test)

#4
##Train Results
print("Train Results -")
pred_kknn = kknn(Spam~., train, X_train, k= 30, kernel = "optimal")
fitted.results = fitted(pred_kknn)
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != Y_train)
print(paste('Train Accuracy : ',1-misClasificError))
tbl3_train = confMat(fitted.results, Y_train)
print(tbl3_train)

##Test Results
print("Test Results -")
pred_kknn = kknn(Spam~., train, X_test, k= 30, kernel = "optimal")
fitted.results = fitted(pred_kknn)
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != Y_test)
print(paste('Test Accuracy : ',1-misClasificError))
tbl3_test = confMat(fitted.results, Y_test)
print(tbl3_test)


#5
##Train Results
print("Train Results -")
pred_kknn = kknn(Spam~., train, X_train, k= 1, kernel = "optimal")
fitted.results = fitted(pred_kknn)
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != Y_train)
print(paste('Train Accuracy : ',1-misClasificError))
tbl4_train = confMat(fitted.results, Y_train)
print(tbl4_train)

##Test Results
print("Test Results -")
pred_kknn = kknn(Spam~., train, X_test, k= 1, kernel = "optimal")
fitted.results = fitted(pred_kknn)
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != Y_test)
print(paste('Test Accuracy : ',1-misClasificError))
tbl4_test = confMat(fitted.results, Y_test)
print(tbl4_test)


#Assignment 3
#1
my_lm = function(X, Y){#, X_cv, Y_cv){
  X = as.matrix(X)
  Y = as.matrix(Y)
  W = ginv(t(X)%*%X)%*%t(X)%*%Y
  return(W)
}

my_predict = function(W, X_test, Y_test=NULL){
  if(is.null(Y_test)){
    X_test = as.matrix(X_test)
    pred_Y = X_test%*%W
    return(pred_Y)
  }else{
    X_test = as.matrix(X_test)
    Y_test = as.matrix(Y_test)
    pred_Y = X_test%*%W
    sdif = (Y_test - pred_Y)^2
    loss = sum(sdif)/nrow(Y_test)
    return(loss)
  }
}

bsSel = function(X, Y, folds){
  set.seed(12345)
  seq_costs = matrix(0, nrow = 0, ncol = 3)
  nc = ncol(X)
  #Choosing best subset
  for(k in 1:nc){
    combs = combn(1:nc, k)
    for(j in 1:ncol(combs)){
      c = combs[, j]
      X_sub <- X[, c]
      X_sub <- cbind(X_sub, 1)
      
      #calculating the K folds cv loss
      set.seed(12345)
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
        W = my_lm(X_train, Y_train)
        cv_loss = my_predict(W, X_cv, Y_cv)
        cvl = cvl + cv_loss
      }
      loss = cvl/folds
      #end of calc
      
      seq = paste(c, collapse = ",")
      seq_costs = rbind(seq_costs, c(seq, loss, k))
    }
  }
  seq_costs = as.data.frame(seq_costs)
  colnames(seq_costs) = c("Sequence", "CV_Loss", "Num_Parameters")
  seq_costs$CV_Loss = as.numeric(as.character(seq_costs$CV_Loss))
  return(seq_costs)
}

#2
data = swiss
head(data)
Y = data$Fertility
X = data[,2:ncol(data)]

best = bsSel(X,Y, 5)
best_seq = best[which.min(best$CV_Loss),]
print(best_seq)

ggplot(best, aes(x=best$Num_Parameters, y=best$CV_Loss)) + geom_point()

#Assignment 4
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

#3
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


dim(train)
dim(X_test)
losses
los = data.frame(losses)
colnames(los)=c("Train_Loss", "Test_Loss")
los$Param = rownames(los)
los

ggplot(los)  + geom_line(aes(Param, Train_Loss, group=1), col='red') + geom_line(aes(Param, Test_Loss, group=2), col='blue')


#4
data = tecator_data
data$Moisture = NULL
data$Sample = NULL
data$Protein = NULL


tecatordata<-data[,c(1:101)]
lmmodel <- glm(Fat~., data = tecatordata,family=gaussian)
model_AIC<-stepAIC(lmmodel,trace=FALSE,direction = "both")

#length(attr(terms(model_AIC),"term.labels"))
attr(terms(model_AIC),"term.labels")

#5
Y = as.matrix(data["Fat"])
X = as.matrix(data[,-ncol(data)])

ridgereg<-glmnet(X,Y,alpha=0,family ="gaussian")

plot(ridgereg, "lambda", label=TRUE)

#6
set.seed(12345)
lassoreg<-glmnet(X,Y,alpha=1,family ="gaussian")

lam<-lassoreg$lambda
plot(lassoreg, "lambda" ,label=TRUE)

#7
lam<-c(lam,0)
set.seed(12345)
lassreg_cv<-cv.glmnet(X,Y,alpha=1,family="gaussian",lambda = lam)
plot(lassreg_cv)
bestlam = lassreg_cv$lambda.1se

z<-as.matrix(coef(lassreg_cv,s="lambda.1se"))
z<-as.matrix(z[!rowSums(z==0),])

paste("Best lambda:" ,bestlam)

#8
