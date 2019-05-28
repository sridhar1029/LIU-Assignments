library(ggplot2)

#1
load("chemical.RData")
ggplot() + geom_line(aes(X, Y), col="red")

#2
n = length(X)
mu = replicate(n, 0)
mu[1] = 1
sd = sqrt(0.2)
for(i in 2:n){
  mu[i] = rnorm(1, mu[i-1], sd)
}

Y_prior = replicate(n, 0)
for(i in 1:n){
  Y_prior[i] = rnorm(1, mu[i], sd)
}
ggplot() + geom_line(aes(X, Y_prior), col="red")


#4
f.MCMC.Gibbs<-function(nstep, Y){
  n = length(Y)
  mu = matrix(0, nrow = nstep, ncol = n)
  mu[1, ] = rep(0,n)
  for (i in 2:nstep){
    last_mu = mu[i-1, ]
    new_mu = mu[i, ]
    for(j in 1:n){
      mu_new = ifelse(j==1, (Y[1] + last_mu[2])*0.5, 
                      ifelse(j==n, (Y[n] + new_mu[n-1])*0.5,
                             (Y[j]+new_mu[j-1]+last_mu[j+1])/3 ))
      var_new = ifelse(i==1, 0.2/2,
                       ifelse(i==n, 0.2/2,
                              0.2/3))
      new_mu[j] = rnorm(1, mu_new, sqrt(var_new))
    }
    mu[i, ] = new_mu
  }
  return(mu)
}

mus = f.MCMC.Gibbs(nstep = 1000, Y)
mu_avg = colSums(mus)/1000
ggplot() + geom_point(aes(X, Y), col="red") + geom_line(aes(X, mu_avg), col="blue")


#5 Trace plot
ggplot() + geom_line(aes(1:1000, mus[,50]))