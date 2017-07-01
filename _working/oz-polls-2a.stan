// oz-polls-2a.stan

data {
  int<lower=1> n_days;            // number of days
  real mu_start;                  // value at starting election
  real mu_finish;                 // value at final election
  int<lower=1> y_n;               // number of polls
  vector[y_n] y_values;             // actual values in polls
  int<lower=0> y_days[y_n];       // the number of days since starting election each poll was taken
  vector[y_n] y_ss;               // sampling sizes
}
parameters {
  vector[n_days] mu;               // underlying state of vote intention
}

model {
  // calculate the standard errors of polls on the given day and size:
  vector[n_days] p;
  vector[y_n] y_se;
  p = inv_logit(mu);
  for(i in 1:y_n){
     y_se[i] = sqrt(2 * p[y_days[i]] * (1-p[y_days[i]]) / y_ss[i]); // "2" is to double variance due to total survey error
  }
  
  
  // state model
  mu[1] ~ normal(mu_start, 0.001);
  
  mu[2:n_days] ~ normal(mu[1:(n_days - 1)], 0.01);
      
  // measurement model
  // 1. Election result
  
  mu_finish ~ normal(mu[n_days], 0.001);
  
  // 2. Polls
  y_values ~ normal(inv_logit(mu[y_days]), y_se);
  
}
