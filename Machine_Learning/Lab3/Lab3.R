library(ggplot2)
library(geosphere)

#Assignment 1
kernal_methods <- function(st, date, lat, lon, time_seq, h_date, h_time, h_distance){
  st$h_dist = abs(distHaversine(p1 = c(lon, lat), p2 = st[,c("longitude", "latitude")]))
  st$h_dist = exp(-(st$h_dist/h_distance)^2)
  st$h_date = as.numeric(difftime(date, st$date, units = c("days")))
  st$h_date = ifelse(st$h_date>0, st$h_date, 0)
  st = subset(st, st$h_date!=0)
  st$h_date = exp(-(st$h_date/h_date)^2)
  times = c()
  for(t in 1:length(time_seq)){
    d = as.Date(time_seq[t])
    c_time = format(time_seq[t], "%H:%M:%S")
    times = append(times, c(c_time))
    st[c_time] =  as.numeric(abs(difftime(strptime(paste(d, c_time),
                                                   "%Y-%m-%d%H:%M:%S"),
                                          strptime(paste(d, st$time),
                                                   "%Y-%m-%d%H:%M:%S"),
                                          units = c("hour"))))
    st[c_time] = exp(-(st[c_time]/h_time)^2)
  }
  temp = st[times]
  temp_add_d = temp + (st$h_date + st$h_dist)
  temp_mul_d = temp * (st$h_date * st$h_dist)
  temp_add_n = temp_add_d*st$air_temperature
  temp_mul_n = temp_mul_d*(st$air_temperature)
  
  temp_add = colSums(temp_add_n)/colSums(temp_add_d)
  temp_mul = colSums(temp_mul_n)/colSums(temp_mul_d)
  
  d = data.frame(Index = 1:length(times), Time = times, Add = temp_add, Mul = temp_mul)
  #ggplot(d) + geom_line(aes(Index, Add, col="Add")) + geom_line(aes(Index, Mul, col="Mul"))
  return(d)
}

set.seed(1234567890)
stations = read.csv("stations.csv")
temps = read.csv("temps50k.csv")
st <- merge(stations,temps,by="station_number")
colnames(st)
h_distance <- 30000
h_date <- 12
h_time <- 4
lat <- 58.4274 # The point to predict (up to the students)
lon <- 14.826
date <- "2014-02-04" # The date to predict (up to the students)
start <- as.POSIXct(date)
interval <- 60
end <- start + as.difftime(1, units="days")
time_seq <- seq(from=start, by=interval*120, to=end)
time_seq = time_seq[3:length(time_seq)]

pred = kernal_methods(st, date, lat, lon, time_seq, h_date, h_time, h_distance)
ggplot(d) + geom_line(aes(Index, Add, col="Add")) + geom_line(aes(Index, Mul, col="Mul"))
#Assignment 2
library(kernlab)
data(spam)

index <- sample(1:dim(spam)[1])
spamtrain <- spam[index[1:floor(dim(spam)[1]/2)], ]
spamtest <- spam[index[((ceiling(dim(spam)[1]/2)) + 1):dim(spam)[1]], ]


filter1 <- ksvm(type~.,data=spamtrain,kernel="rbfdot",
                kpar=list(sigma=0.05),C=0.5,cross=3)
filter1
mailtype1 <- predict(filter1,spamtest[,-58])
table(mailtype1,spamtest$type)

filter2 <- ksvm(type~.,data=spamtrain,kernel="rbfdot",
                kpar=list(sigma=0.05),C=1,cross=3)
filter2
mailtype2 <- predict(filter2,spamtest[,-58])
table(mailtype2,spamtest$type)


filter3 <- ksvm(type~.,data=spamtrain,kernel="rbfdot",
               kpar=list(sigma=0.05),C=5,cross=3)
filter3
mailtype3 <- predict(filter3,spamtest[,-58])
table(mailtype3,spamtest$type)

print("First Model")
table(mailtype1,spamtest$type)
sum(mailtype1==spamtest$type)
print("Second Model")
table(mailtype2,spamtest$type)
sum(mailtype2==spamtest$type)
print("Third Model")
table(mailtype3,spamtest$type)
sum(mailtype3==spamtest$type)
