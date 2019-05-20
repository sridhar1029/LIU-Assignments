library(readxl)

#SP1

k_near = function(x, k){
  ind = sort(x, decreasing = F, index.return=T)$ix[1:k]
  return(ind)
}

knearest = function(train, k, test){
  X_train = train[, 1:(ncol(train)-1)]
  Y_train = train$Spam
  X_test = test[, 1:(ncol(test)-1)]
  Y_test = test$Spam
  a = as.matrix(X_train)
  b = as.matrix(X_test)
  
  a_div = rowSums(a^2)^(0.5)
  a_cap = a / a_div
  
  b_div = rowSums(b^2)^(0.5)
  b_cap = b / b_div
  
  c = a_cap %*% t(b_cap)
  
  d = 1-c
  
  res = matrix(0, nrow = k, ncol = 0)
  for( i in 1:ncol(d)){
    ind = k_near(d[, i], k)
    res = cbind(res, Y_train[ind])
  }
  res = t(res)
  pred = rowSums(res)/k
  # res_df = data.frame(res[2:nrow(res),])
  return(pred)
}

spam_data = read_xlsx("spambase.xlsx", sheet = "spambase_data")
head(spam_data)

## 50% of the sample size
smp_size <- floor(0.50 * nrow(spam_data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(spam_data)), size = smp_size)

train <- spam_data[train_ind, ]
test <- spam_data[-train_ind, ]
print(dim(train))
dim(test)

#train
#Train
pred = knearest(train, 30, train)
y_pred = ifelse(pred>0.5, 1, 0)
acc = sum(y_pred==train$Spam)
print("Train Results -")
print(acc/nrow(train))

pos = which(train$Spam == 1)
tn = sum(y_pred[pos])
fn = length(pos) - tn
neg = which(train$Spam == 0)
fp = sum(y_pred[neg])
tp = length(neg) - fp
tbl1 = data.frame('not_spam'= c(tp, fn), 'spam'=c(fp, tn), row.names = c('not_spam', 'spam'))
print(tbl1)

#Test
pred = knearest(train, 30, test)
y_pred = ifelse(pred>0.5, 1, 0)
acc = sum(y_pred==test$Spam)
acc/nrow(test)

pos = which(test$Spam == 1)
tn = sum(y_pred[pos])
fn = length(pos) - tn
neg = which(test$Spam == 0)
fp = sum(y_pred[neg])
tp = length(neg) - fp
tbl1 = data.frame('not_spam'= c(tp, fn), 'spam'=c(fp, tn), row.names = c('not_spam', 'spam'))
print(tbl1)


#SP2
data = cars
kth_near = function(X, val, k){
  d = abs(X - val)
  dis = sort(d, decreasing = F, index.return=T)$x[k]
  return(dis)
}

v_cal = function(d){
  v = (pi^(d/2))/(factorial(d/2))
  return(v)
}

pcap_knn = function(X, k, d, val){
  p_knn = k/(length(X)*v_cal(d)*(kth_near(X, val, k))^d)
  return(p_knn)
}

X = cars$speed
k=6
d=1
a = matrix(0, nrow =0, ncol = 2)
for(i in seq(min(X),max(X),0.99)){
  p = pcap_knn(X, k, d, i)
  a = rbind(a, c(i, p))
}
a = data.frame(a)
colnames(a) = c('X', 'PDF')

hist(X, prob=TRUE, col="grey", breaks = 10)
lines(a, col="blue", lwd=2) 
#lines(a, lty="dotted", col="darkgreen", lwd=2) 