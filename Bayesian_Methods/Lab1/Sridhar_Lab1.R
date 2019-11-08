library(ggplot2)

#A1
n = 20; s = 14; alp0 = 2; bta0 = 2;
alp1 = alp0 + s; bta1 = bta0 + n - s;

theta = seq(0, 1, 0.01)
posterior_dist = pbeta(theta, alp1, bta1)

draw_10 = rbeta(10, alp1, bta1)
draw_100 = rbeta(100, alp1, bta1)
draw_1000 = rbeta(1000, alp1, bta1)
draw_10000 = rbeta(10000, alp1, bta1)

ggplot() + 
  geom_density(aes(draw_10, col="10"),size=1) + 
  geom_density(aes(draw_100, col="100"),size=1) + 
  geom_density(aes(draw_1000, col="1000"),size=1) +
  geom_density(aes(draw_10000, col="10000"),size=1) +
  geom_line(aes(theta, posterior_dist, col="True Posterior"), size=1)
  xlim(0, 1)

#A3
mu = 2.39
y = c(-2.44, 2.14, 2.54, 1.83, 2.02, 2.33, -2.79, 2.23, 2.07, 2.02)
prior_a3 = function(k){
  return(exp(-k))
}
likelihood_a3 = function(y, k, mu){
  numer = exp(k*(sum(cos(y - mu))))
  dinom = (2 * pi * besselI(k, 0))^10
  return(numer/dinom)
}

k = seq(0, 10, 0.01)
vals = data.frame(k, prior=prior_a3(k), likelihood=likelihood_a3(y, k, mu))
vals$posterior = vals$prior*vals$likelihood
prior_sum = sum(vals$prior)
likel_sum = sum(vals$likelihood)
postr_sum = sum(vals$posterior)
vals$prior = vals$prior/prior_sum
vals$likelihood = vals$likelihood/likel_sum
vals$posterior = vals$posterior/postr_sum

hpd = vals
hpd = hpd[order(hpd$posterior, decreasing = TRUE),]
hpd$cumPost = cumsum(hpd$posterior)
max(hpd$cumPost)
hpd_vals = which(hpd$cumPost<0.95)
hpd_cut = length(hpd_vals) + 1

plot(vals$k, vals$prior, 'l', col=2)
lines(vals$k, vals$likelihood, 'l', col=3)
lines(vals$k, vals$posterior, 'l', col=4)
abline(h=hpd[hpd_cut,]$posterior, col=5)

#A2
#a
library(LaplacesDemon)
set.seed(12345)
mu = 3.5
y = c(14, 25, 45, 25, 30, 33, 19, 50, 34, 67)
n = 10

tow = sum((log(y) - mu)^2)/n

x = seq(0.1, 5, 0.01)
theo_inv_chi = dinvchisq(x, n, tow)
simu_inv_chi = rinvchisq(10000, n-1, tow)
ggplot() + 
  geom_line(aes(x, theo_inv_chi, col="Theoritical"), size=1) + 
  geom_density(aes(simu_inv_chi, col="Simulated"), size=1)

my_inv_chisquare <- function(n, df, tow){
  X <- rchisq(n, df)
  return(((df)*tow)/X)
}
plot(density(my_inv_chisquare(10000, 9, tow)))
lines(density(rinvchisq(10000, 9, tow)))

#b
g_vals = (2*pnorm(sqrt(simu_inv_chi/2))) - 1
ggplot() + geom_density(aes(g_vals))

#c
g_dens = density(g_vals)
data_for_hpd = data.frame(giniVals = g_dens$x, giniDens = g_dens$y)
data_for_hpd$giniDens = data_for_hpd$giniDens/sum(data_for_hpd$giniDens)
data_for_hpd = data_for_hpd[order(data_for_hpd$giniDens, decreasing = TRUE), ]
data_for_hpd$cumPost = cumsum(data_for_hpd$giniDens)
max(data_for_hpd$cumPost)
hpd_vals = which(data_for_hpd$cumPost<0.95)
hpd_cut = length(hpd_vals)
ggplot() + geom_line(aes(data_for_hpd$giniVals, data_for_hpd$giniDens)) + 
  geom_hline(yintercept = data_for_hpd$giniDens[hpd_cut], col="red") +
  geom_vline(xintercept = data_for_hpd$giniVals[hpd_cut-1], col="red") +
  geom_vline(xintercept = data_for_hpd$giniVals[hpd_cut-2], col="red") +
  xlab("Gini Values") + 
  ylab("Density")




## Test
N = 20
s = 14
a = 10
b = 1
lik_test = function(theta, N, s){
  return(theta^s * (1 - theta)^(N - s))
}

prior_test = function(theta, a, b){
  const = factorial(a + b - 1)/(factorial(a-1)*factorial(b-1))
  return(const*(theta^(a-1) * ((1-theta)^(b-1))))
}

post_test = function(theta, N, s, a, b){
  const = factorial(N + a + b - 1)/(factorial(s + a - 1)*factorial(N + b - s - 1))
  return(const*(theta^(s+a-1) * ((1-theta)^(N+b-s-1))))
}

theta_test = seq(0, 1, 0.01)

results_test = data.frame(tta = theta_test,
                          pri = prior_test(theta_test, a, b),
                          lik = lik_test(theta_test, N, s),
                          pos = post_test(theta_test, N, s, a, b))

results_test2 = results_test
results_test2$pri = results_test$pri/sum(results_test$pri)
results_test2$lik = results_test$lik/sum(results_test$lik)
results_test2$pos = results_test$pos/sum(results_test$pos)

ggplot(results_test2) + geom_line(aes(tta, pri, col="prior")) +
  geom_point(aes(tta, lik, col="likelihood")) + 
  geom_line(aes(tta, pos, col="posterior"))
