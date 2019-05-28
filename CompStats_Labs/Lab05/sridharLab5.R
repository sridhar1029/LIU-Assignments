library(ggplot2)
library(readxl)
library(boot)

set.seed(12345)

lottery_data = read_xls("lottery.xls")
head(lottery_data)
lot_data = data.frame("X"=lottery_data$Day_of_year, "Y"=lottery_data$Draft_No)
head(lot_data)

#1
ggplot(lot_data) + geom_point(aes(X, Y))
#looks random to me

#2
loessMod <- loess(Y ~ X, data=lot_data)
loessMod10 <- loess(Y ~ X, data=lot_data, span=0.10) # 10% smoothing span
loessMod25 <- loess(Y ~ X, data=lot_data, span=0.25) # 25% smoothing span
loessMod50 <- loess(Y ~ X, data=lot_data, span=0.50) # 50% smoothing span

smoothed <- predict(loessMod) 
smoothed10 <- predict(loessMod10) 
smoothed25 <- predict(loessMod25) 
smoothed50 <- predict(loessMod50)

ggplot() + geom_point(aes(lot_data$X, lot_data$Y)) + 
  geom_line(aes(lot_data$X, smoothed10), col="red", size=1) + 
  geom_line(aes(lot_data$X, smoothed25), col="blue", size=1) + 
  geom_line(aes(lot_data$X, smoothed50), col="green", size=1) +
  geom_line(aes(lot_data$X, smoothed), col="grey", size=1) 

#3
BootstrapT_Test = function(data_boot=lot_data, id){
  data_boot$boot_X = data_boot$X[id]
  data_boot$boot_Y = data_boot$Y[id]
  loessMod_boot <- loess(boot_Y ~ boot_X, data=data_boot)
  smoothed_boot <- predict(loessMod_boot)
  
  Xb = data_boot$boot_X[which.max(data_boot$boot_Y)]
  Xa = data_boot$boot_X[which.min(data_boot$boot_Y)]
  Ycap_Xb = smoothed_boot[Xb]
  Ycap_Xa = smoothed_boot[Xa]
  t_val = (Ycap_Xb - Ycap_Xa)/(Xb-Xa)
  return(t_val)
}
N = 2000
boot.out = boot(lot_data, BootstrapT_Test, N)
boot.out

hist(boot.out$t, breaks = 100)
lines(boot.out$t0, 400, col="red", type="h", lwd=2)
lines(-boot.out$t0, 400, col="red", type="h", lwd=2)
mean(boot.out$t)
sd(boot.out$t)
pnorm(0, mean = mean(boot.out$t), sd = sd(boot.out$t))

boot.ci(boot.out, type = "norm")

#calc p value
new_diff = abs(boot_var$t) - abs(boot_var$t0)
hist(new_diff, breaks = 100)
lines(0, 500, type = 'h', col="red")

permTest = function(boot_var){
  new_diff = abs(boot_var$t) - abs(boot_var$t0)
  sum(new_diff<0)/2000
}
permTest(boot.out)

mean(abs(boot.out$t) >= abs(boot.out$t0))

#5
non_rand_data = lot_data
alphas_nr = seq(0.1, 10, 0.1)
p_vals_nr = matrix(0, nrow = length(alphas_nr), ncol = 3)
p_vals_nr[,1] = alphas_nr
for(j in 1:length(alphas_nr)){
  alpha_non_rand = alphas_nr[j]
  beta_non_rand = rnorm(1, mean = 183, sd = 10)
  p_vals_nr[j,2] = beta_non_rand
  for(i in 1:366){
    non_rand_data$Y[i] = max(0, min(alpha_non_rand*i + beta_non_rand, 366)) 
  }
  head(non_rand_data)
  ggplot(non_rand_data) + geom_point(aes(X, Y))
  
  N = 200
  boot.out_nr = boot(non_rand_data, BootstrapT_Test, N)
  #boot.out_nr
  #hist(boot.out_nr$t, breaks = 100)
  #mean(boot.out_nr$t)
  #sd(boot.out_nr$t)
  #pnorm(1, mean = mean(boot.out_nr$t), sd = sd(boot.out_nr$t))
  
  #calc p value
  #diff_est = boot.out_nr$t - boot.out_nr$t0
  #p_vals_nr[j, 3] = sum(diff_est>0)/2000
  p_vals_nr[j, 3] = permTest(boot.out_nr)
}
plot(1-p_vals_nr[,2], p_vals_nr[,3])
plot(p_vals_nr[,1], p_vals_nr[,2])
hist(p_vals_nr[,3])
