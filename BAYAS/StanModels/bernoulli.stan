data {
  int<lower=0> N;
  array[N] int<lower=0, upper=1> y;
}
parameters {
  real<lower=0, upper=1> p;
}
model {
  y ~ bernoulli(p);
  p ~ beta(2,2);
}
generated quantities {
  vector[N] y_rep;
  for(i in 1:N){
    y_rep[i] = bernoulli_rng(p);
  }
}
