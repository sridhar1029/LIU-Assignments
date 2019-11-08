data {
  int<lower=0> T;
  vector[T] y;
}

parameters {
  real alpha;
  real<lower=-1,upper=1> beta;
  real<lower=0> sigma;
}

model {
  y[2:T] ~ normal(alpha + beta*y[1:(T-1)], sigma);
}
