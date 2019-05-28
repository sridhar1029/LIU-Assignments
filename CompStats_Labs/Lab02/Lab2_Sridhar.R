library(ggplot2)
#Ass 1
#Q1
mortRate = read.csv("mortality_rate.csv", sep=';', dec = ',')
mortRate$LMR = log(mortRate$Rate)
head(mortRate)

n = dim(mortRate)[1]
set.seed(123456)
id = sample(1:n, floor(n*0.5))
train = mortRate[id,]
test = mortRate[-id,]

#Q2
my_MSE = function(lambda, pars, iterCounter=FALSE){
  model = loess(pars$Y~pars$X, enp.target = lambda)
  fYpred = predict(model, newdata = pars$Xtest)
  n_test = length(pars$Ytest)
  predictiveMSE = sum((pars$Ytest - fYpred)^2)/n_test
  if(iterCounter){
    if(!exists("iterForMyMSE")){
      assign("iterForMyMSE",
             value = 1,
             globalenv())
    } else {
      currentNr <- get("iterForMyMSE")
      assign("iterForMyMSE",
             value = currentNr + 1,
             globalenv())
    }
  }
  return(predictiveMSE)
}

#Q3
lambda = seq(0.1,40,by=0.1)
pars = list()
pars$X = train[, 1]
pars$Y = train[, 3]
pars$Xtest = test[, 1]
pars$Ytest = test[, 3]

predMSE = matrix(0, ncol = 1, nrow = length(lambda))
counts = matrix(0, ncol = 1, nrow = length(lambda))
for(i in 1:length(lambda)){
  iterForMyMSE = 0
  predMSE[i] = my_MSE(lambda[i], pars, TRUE)
  counts[i] = iterForMyMSE
}

min_lbd = lambda[which.min(predMSE)]
min_mse = min(predMSE)

ggplot() + geom_line(aes(lambda, predMSE)) + geom_point(aes(min_lbd, min_mse), col='red')

iterForMyMSE = 0
val = optimise(my_MSE, interval = lambda, pars=pars, iterCounter=TRUE)
iterForMyMSE
val

#Q4
iterForMyMSE = 0
val = optimise(my_MSE, interval = lambda, pars=pars, iterCounter=TRUE, tol = 0.01)
iterForMyMSE
val

#Q5
iterForMyMSE = 0
optim(0.1, fn=my_MSE, pars=pars, iterCounter=TRUE, method="BFGS")
iterForMyMSE




##Ass2
#Q1
