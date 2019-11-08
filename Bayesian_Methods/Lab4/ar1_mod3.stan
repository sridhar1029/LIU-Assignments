data {
  int<lower=0> T;
  int c[T];
}

parameters {
  real alpha;
  real<lower=-1,upper=1> beta;
  real<lower=0> sigma;
  vector[T] x;
}

model {
  alpha ~ normal(0, 10000);
  beta ~ uniform(-1, 1);
  sigma ~ exponential(100);
  
  x[2:T] ~ normal(alpha + beta*x[1:(T-1)], sigma);
  c ~ poisson(exp(x));
}
