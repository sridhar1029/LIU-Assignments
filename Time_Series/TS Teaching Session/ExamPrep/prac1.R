#Chapter 3

createData = function(size=10){
  a = data.frame(x1=rnorm(100))
  for (i in 2:size) {
    a[paste("x", i, sep = "")] = rnorm(100)
  }
  return(a)
}

dat = createData(50)

matplot(dat, type="l", lty="solid", col=gray(0.5, 0.4))
lines(1:100, rnorm(100), col="blue")


matplot(apply(dat, 2, cumsum), type="l", lty="solid", col=gray(0.5, 0.4))
lines(1:100, cumsum(rnorm(100)), col="blue")


#Chapter 4

library(forecast)
library(datasets)

data("nhtemp")
data("co2")
head(co2)
co2

co2_ts = ts(as.vector(co2), frequency = 12, start = 1959)
ts.plot(co2_ts)
temp_ts = ts(nhtemp, frequency = 1, start=1912)
ts.plot(temp_ts)

#DECOMPOSING TimeSeries

#R has built in function filter to calculate moving averages and other linear filters

#weights for moving averages
fltr = c(1/2, rep(1, times=11), 1/2)/12
#estimation of trend
co2.trend = filter(co2_ts, filter = fltr, method = "convo", sides = 2)
#plot the trend
plot.ts(co2.trend)

#seasonal effect over time 
co2.1T = co2_ts - co2.trend
plot.ts(co2.1T)

ll = length(co2.1T)
ff = frequency(co2.1T)
periods = ll %/% ff
#index of cummulative month
index = seq(1, ll, by = ff) - 1
#get means by month
mm = numeric(ff)
for (i in 1:ff) {
  mm[i] = mean(co2.1T[index+i], na.rm = T)
}
mm
#subtract mean to make overall mean zero
(mm = mm - mean(mm))
#plot monthly seasonal effects
plot.ts(mm)

#creating object for season
co2.seas = ts(rep(mm, periods+1)[seq(ll)], start = start(co2.1T), frequency = ff)
plot.ts(co2.seas)

#Random errors over time(e_t)
co2.err = co2_ts - co2.trend - co2.seas
plot.ts(co2.err)

#all four plots together
plot(cbind(co2_ts, co2.trend, co2.seas, co2.err), main="", yax.flip = T)


#using decompose function
co2.decompose = decompose(co2_ts)

plot(cbind(co2.decompose$x, 
           co2.decompose$trend, 
           co2.decompose$seasonal, 
           co2.decompose$random), main="", yax.flip = T)
plot(co2.decompose, yax.flip=T)

#For a multiplicative model pass in the argument type="multiplicative"
#default is additive



#DIFFERENCING TimeSeries

#Removing non-linear trend
co2.d2 = diff(co2_ts, differences = 2)
plot(co2.d2)

#Removing seasonal effect
co2.d2.d12 = diff(co2.d2, lag = 12)
plot.ts(co2.d2.d12)


#ACF

acf(co2_ts, lag.max = 36)

plot.acf = function(ACFobj){
  rr = ACFobj$acf[-1]
  kk = length(rr)
  nn = ACFobj$n.used
  ci = -1/nn+c(-2, 2)/sqrt(nn)
  plot(seq(kk), rr, type = "h", lwd=2, yaxs="i", xaxs="i",
       ylim = c(min(ci, rr)-0.2, 1), xlim = c(0, kk+1),
       xlab="Lag", ylab="Correlation", las=1)
  abline(h=ci, lty="dashed", col="blue")
  abline(h=0)
}

plot.acf(acf(co2_ts, lag.max = 36))
plot.acf(acf(co2.d2.d12, lag.max = 36))


plot.ts(seq(100))
plot.acf(acf(seq(100), plot = F))

#PACF
pacf(co2_ts)

plot.pacf = function(PACFobj){
  rr = PACFobj$acf
  kk = length(rr)
  nn = PACFobj$n.used
  ci = -1/nn+c(-2, 2)/sqrt(nn)
  plot(seq(kk), rr, type = "h", lwd=2, yaxs="i", xaxs="i",
       ylim = c(min(ci, rr)-0.2, 1), xlim = c(0, kk+1),
       xlab="Lag", ylab="Correlation", las=1)
  abline(h=ci, lty="dashed", col="blue")
  abline(h=0)
}
plot.pacf(pacf(co2_ts, lag.max = 36))


#CCF

suns = ts.intersect(lynx, sunspot.year)[, "sunspot.year"]
lynx = ts.intersect(lynx, sunspot.year)[, "lynx"]
plot(cbind(suns, lynx), yax.flip = T)

ccf(suns, log(lynx), ylab="Cross-Correlation")

#Simulating arima
ar2 = arima.sim(n = 1000, model = list(order(2,0,0), ar=c(0.6, 0.3), sd=1))
ts.plot(ar2)

ma2 = arima.sim(n = 1000, model = list(order(0,0,2), ma=c(0.6, 0.3), sd=0.1))
ts.plot(ma2)

ar2ma2 = arima.sim(n = 1000, model = list(order(2,0,2), ar=c(0.6, 0.3), ma=c(0.5, 0.9), sd=1))
ts.plot(ar2ma2)
acf(ar2ma2)
pacf(ar2ma2)
ar2ma2_diff = diff(ar2ma2, differences = 1)
acf(ar2ma2_diff)
pacf(ar2ma2_diff)
ts.plot(ar2ma2_diff)



#CH5

library(reshape2)
library(ggplot2)

TT = 100
nsim = 10
ys = matrix(0, nrow = TT, ncol = nsim)
for (i in 1:nsim) ys[,i] = as.vector(arima.sim(TT, model = list(ar=0.8)))
ys = data.frame(ys)
ys$id = 1:TT
ys2 = melt(ys, id.vars = "id")
ggplot(ys2, aes(x=id, y=value, group=variable)) + geom_line()
