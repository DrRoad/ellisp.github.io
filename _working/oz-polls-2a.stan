// oz-polls-2a.stan

data {
  int<lower=1> n_days;            // number of days
  real mu_start;                  // value at starting election, on logit scale
  real mu_finish;                 // value at final election, on logit scale
  int<lower=1> y_n;               // number of polls
  vector[y_n] y_values;             // actual values in polls, on [0,1] scale
  int<lower=0> y_days[y_n];       // the number of days since starting election each poll was taken
  vector[y_n] y_se;               // sampling error
  real<lower=1> inflator;         // how much to multiply standard errors by
}
parameters {
  vector[n_days] epsilon;         // innovations in the MA process
  real beta;                      // moving average slope
}

transformed parameters {
  vector[n_days] mu;               // underlying state of vote intention, on logit scale
  mu[1] = mu_start;
  for(i in 2:n_days){
    mu[i] = mu[i-1] + beta * epsilon[i-1] + epsilon[i];
  }
}

model {
  
  // priors for MA model
  beta ~ normal(0, 1);        // multiplier of yesterday's innovation
  
  // state model
  epsilon ~ normal(0, 0.01);   // innovations on logit scale
      
  // measurement model
  // 1. Election result
  mu_finish ~ normal(mu[n_days], 0.001);  // final election result on logit scale
  
  // 2. Polls
  y_values ~ normal(inv_logit(mu[y_days]), y_se * inflator); // poll results on their original scale
  
}
