library(ggplot2)
library(readxl)
library(mgcv)
library(pamr)

#Assignment 1
##1
influenza = read_xlsx("Influenza.xlsx")
head(influenza)
ggplot(influenza) + geom_line(aes(Time, Influenza))
ggplot(influenza) + geom_line(aes(Time, Mortality))
ggplot(influenza) + geom_line(aes(Time, Influenza, col="red")) + 
  geom_line(aes(Time, Mortality, col="blue"))

##2
model = gam(Mortality~Year+s(Week, k= length(unique(influenza$Week))), data = influenza, family = gaussian, method = "GCV.Cp")
plot(model, pages = 1, residuals = T)
plot(model, pages = 1, seWithMean = T)
summary(model)
plot(influenza$Week, influenza$Mortality)

##3
data = influenza[,c("Year","Week")]
data1 = influenza[,c("Time","Mortality")]
pred = predict(model, data)
data1$Pred = pred

plot(influenza$Time, pred, col="red")
points(influenza$Time, influenza$Mortality, col="blue")

ggplot(data1) + geom_line(aes(Time, Mortality, col="red"))+
  geom_line(aes(Time, Pred, col="blue"))
#plot the spline component and interpret the plot

#4
q = seq(0,1,0.001)
res = matrix(0, nrow = 0, ncol = 2)
n = nrow(influenza)
for( i in q){
  model_i = gam(Mortality~Year+s(Week, k= length(unique(influenza$Week)), sp=i),
                data = influenza, family = gaussian, method = "GCV.Cp")
  mdf = sum(model_i$edf)
  df = n - mdf
  res = rbind(res, c(i, mdf))
}
res = as.data.frame(res)
colnames(res) = c("Lambda", "DOF")
plot(res)

#5
plot(data1$Time, model$residuals)
points(data1$Time, influenza$Influenza, col="red")

#6
model2 = gam(Mortality~s(Year, k= length(unique(influenza$Year))) +
               s(Week, k= length(unique(influenza$Week))) + 
               s(Influenza, k= length(unique(influenza$Influenza))),
             data = influenza, 
             family = gaussian, 
             method = "GCV.Cp")
summary(model2)


plot(model2)
data = influenza[,c("Year","Week","Influenza")]
data1 = influenza[,c("Time","Mortality")]
pred = predict(model2, data)
data1$Pred = pred

plot(influenza$Time, pred, col="red")
points(influenza$Time, influenza$Mortality, col="blue")

ggplot(data1) + geom_line(aes(Time, Mortality, col="red"))+
  geom_line(aes(Time, Pred, col="blue"))


##Assignment 2
#1
data <- read.csv(file = "data.csv", sep = ";", header = TRUE)

n=NROW(data)
data$Conference <- as.factor(data$Conference)
set.seed(12345) 
id=sample(1:n, floor(n*0.7)) 
train=data[id,] 
test = data[-id,]
rownames(train)=1:nrow(train)
x=t(train[,-4703])
y=train[[4703]]
rownames(test)=1:nrow(test)
x_test=t(test[,-4703])
y_test=test[[4703]]
mydata = list(x=x,y=as.factor(y),geneid=as.character(1:nrow(x)), genenames=rownames(x))
mydata_test = list(x=x_test,y=as.factor(y_test),geneid=as.character(1:nrow(x)), genenames=rownames(x))
model=pamr.train(mydata,threshold=seq(0, 4, 0.1))
cvmodel=pamr.cv(model, mydata)
important_gen <- as.data.frame(pamr.listgenes(model, mydata, threshold = 1.3))
predicted_scc_test <- pamr.predict(model, newx = x_test, threshold = 1.3)

pamr.plotcv(cvmodel)
pamr.plotcen(model, mydata, threshold = 1.3)
