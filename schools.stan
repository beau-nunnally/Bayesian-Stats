data {
  int<lower=0> J;
  real y[J];
  real<lower=0> sigma[J];
}
parameters {
  real mu;
  real<lower=0> tau;
  vector[J] eta;
}
transformed parameters {
  vector[J] theta;
  theta <- mu + tau*eta;
}
model {
  eta ~ normal(0,1);
  y ~ normal(theta,sigma);
}