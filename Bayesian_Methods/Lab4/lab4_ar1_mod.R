library(ggplot2)
library(rstan)

#1A

#Given intial values
mu <- 10
sg_sq <- 2
t_p <- 200   #time points. Number of variables


#function for creating Ar process according to given equation
ar_process <- function(phi,t_p,mu,sg_sq) {
  x <- c()
  x[1] <- mu
  for(i in 2:t_p) {
    x[i] <- mu + phi*(x[i-1]-mu) + rnorm(1,0,sg_sq) 
  }
  return(x)
}


#phi value between -1 and 1
phi <- c(-1,0.7,1)

#Creating data for three phi values
ar_data <- do.call(cbind,lapply(phi,ar_process,t_p=200,mu=10,sg_sq=2))
colnames(ar_data) <- c("samp1","samp2","samp3")
ar_data <- as.data.frame(ar_data)

#plotting to check the effect of phi on Ar process. We need not show these three plots.
ggplot(ar_data,aes(x=seq(1:200))) + 
  geom_line(aes(y=samp1,color="samp1"),size=0.9) + 
  geom_line(aes(y=samp2,color="samp2"),size=0.9) +
  geom_line(aes(y=samp3,color="samp3"),size=0.9) + 
  theme_grey() + 
  scale_color_manual(values = c("#8da0cb", "#fc8d62", "#1b9e77"),
                     labels = c(phi[1],
                                phi[2],
                                phi[3]))


#choosing 0.7 as  avalue to work further on.
ggplot(ar_data,aes(x=seq(1:200))) + 
  geom_line(aes(y=samp2,color="samp2"),size=0.9)+ 
  theme_grey() + 
  scale_color_manual(values = "#fc8d62",labels = phi[2]) +  
  ggtitle(expression(paste("AR process with ",phi,"=0.7"))) +
  xlab("Samples") + ylab("AR process")





#1B
set.seed(12345)
#phi value between -1 and 1
phi <- c(0.3,0.95)
T = 200
mu = 10

#Creating data for three phi values
ar_data <- do.call(cbind,lapply(phi,ar_process,t_p=T,mu=mu,sg_sq=2))
colnames(ar_data) <- c("X","Y")
ar_data <- as.data.frame(ar_data)
head(ar_data)

#plotting to check the effect of phi on Ar process. We need not show these three plots.
ggplot(ar_data,aes(x=seq(1:200))) + 
  geom_line(aes(y=X,color="samp1"),size=0.9) + 
  geom_line(aes(y=Y,color="samp2"),size=0.9) + 
  scale_color_manual(values = c("#8da0cb", "#fc8d62"),
                     labels = c(phi[1], phi[2]))





options(mc.cores=4)

#compile model_x
model <- stan_model("ar1_mod.stan")

#model run on X
fit_x <- sampling(model, list(T=T, y=ar_data$X), iter=1000, chains=4)

params_x <- extract(fit_x)
mu_x = params_x$alpha/(1 - params_x$beta)


library(cowplot)
plot_grid(ggplot() + geom_density(aes(params_x$alpha)),
          ggplot() + geom_density(aes(params_x$beta)),
          ggplot() + geom_density(aes(params_x$sigma)),
          ggplot() + geom_density(aes(mu_x)))

print(fit_x)
a = summary(fit_x)
print(a$summary[1:3,c(1, 4, 8)], digits = 4)

traceplot(fit_x)


quantile(mu_x, probs = c(0.025, 0.975))

#model run on Y
fit_y <- sampling(model, list(T=T, y=ar_data$Y), iter=1000, chains=4)

params_y <- extract(fit_y)

hist(params_y$alpha)
hist(params_y$beta)
hist(params_y$sigma)

print(fit_y)
b = summary(fit_y)
print(b$summary[1:3,c(1, 4, 8)], digits = 4)

traceplot(fit_y)

mu_y = params_y$alpha/(1 - params_y$beta)
hist(mu_y)
quantile(mu_y, probs = c(0.025, 0.975))



#1C
c_data <- as.data.frame(read.table("data.txt", header = T))
hist(c_data$c)
T = length(c_data$c)
model <- stan_model("ar1_mod2.stan")



#model run on C
fit_c <- sampling(model, list(T=T, c=c_data$c), iter=10000, warmup=1000, chains=2)
params_c <- extract(fit_c)

hist(params_c$alpha)
hist(params_c$beta)
hist(params_c$sigma)

c_s = summary(fit_c)
print(c_s$summary[1:4,c(1, 4, 8)], digits = 4)

mu_c = params_c$alpha/(1 - params_c$beta)
hist(mu_c)
quantile(mu_c, probs = c(0.025, 0.975))

x_post = colMeans(params_c$x)
theta_x = exp(x_post)

post_ci = matrix(0, nrow = 140, ncol = 2)
post_mean = matrix(0, nrow = 140, ncol = 1)
#simulate using this theta_x
for(i in 1:140){
  post_theta = rpois(1000, theta_x[i])
  post_ci[i, ] = quantile(post_theta, probs = c(0.025, 0.975))
  post_mean[i,1] = mean(post_theta)
}

ggplot() + 
  geom_line(aes(1:140, c_data$c, col="data")) +
  geom_line(aes(1:140, post_ci[,1], col="95% CI")) +
  geom_line(aes(1:140, post_ci[,2], col="95% CI"))+
  geom_line(aes(1:140, post_mean[,1], col="mean")) 



#1D
model <- stan_model("ar1_mod3.stan")

#model run for d
fit_d <- sampling(model, list(T=T, c=c_data$c), iter=10000, warmup=1000, chains=4)
params_d <- extract(fit_d)

hist(params_d$alpha)
hist(params_d$beta)
hist(params_d$sigma)

d_s = summary(fit_d)
print(d_s$summary[1:4,c(1, 4, 8)], digits = 4)

mu_d = params_d$alpha/(1 - params_d$beta)
hist(mu_d)
quantile(mu_d, probs = c(0.025, 0.975))

x_post_d = colMeans(params_d$x)
theta_x_d = exp(x_post_d)

post_ci_d = matrix(0, nrow = 140, ncol = 2)
post_mean_d = matrix(0, nrow = 140, ncol = 1)
#simulate using this theta_x
for(i in 1:140){
  post_theta = rpois(1000, theta_x_d[i])
  post_ci_d[i, ] = quantile(post_theta, probs = c(0.025, 0.975))
  post_mean_d[i,1] = mean(post_theta)
}

ggplot() + 
  geom_line(aes(1:140, c_data$c, col="data")) +
  geom_line(aes(1:140, post_ci_d[,1], col="95% CI")) +
  geom_line(aes(1:140, post_ci_d[,2], col="95% CI"))+
  geom_line(aes(1:140, post_mean_d[,1], col="mean")) 


ggplot() + 
  geom_line(aes(1:140, post_mean_d[,1], col="mean_d")) +
  geom_line(aes(1:140, post_mean[,1], col="mean_c")) +
  labs(x="Time", y="", title="Comparing the two posteriors")
