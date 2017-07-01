// oz-polls-3a.stan

data {
  int<lower=1> n_days;            // number of days
  real mu_start;                  // value at starting election, on 0-1 scale
  real mu_finish;                 // value at final election, on 0-1 scale
  real inflator;                  // amount by which to inflate the variance of polls
  
  // change the below into 5 matrixes with 3 columns each for values, days, standard error
  int y1_n;                     // number of polls
  int y2_n;
  int y3_n;
  int y4_n;
  int y5_n;
  vector[y1_n] y1_values;       // actual values in polls, in 0-1 scale
  vector[y2_n] y2_values;       
  vector[y3_n] y3_values;       
  vector[y4_n] y4_values;       
  vector[y5_n] y5_values;       
  int y1_days[y1_n];          // the number of days since starting election each poll was taken
  int y2_days[y2_n]; 
  int y3_days[y3_n]; 
  int y4_days[y4_n]; 
  int y5_days[y5_n]; 
  vector[y1_n] y1_ss;             // the sample sizes of the polls
  vector[y2_n] y2_ss;           
  vector[y3_n] y3_ss;           
  vector[y4_n] y4_ss;           
  vector[y5_n] y5_ss;           
}
parameters {
  vector<lower=0,upper=1>[n_days] mu;               // underlying state of vote intention, on 0-1 scale
  real d[5];                                        // polling effects
  real<lower=0> sigma;                              // sd of innovations
}

model {


   // calculate the standard errors of polls on the given day and size:
  vector[y1_n] y1_se;
  vector[y2_n] y2_se;
  vector[y3_n] y3_se;
  vector[y4_n] y4_se;
  vector[y5_n] y5_se;

  for(i in 1:y1_n){
     y1_se[i] = sqrt(inflator * mu[y1_days[i]] * (1 - mu[y1_days[i]]) / y1_ss[i]);  
  }
  for(i in 1:y2_n){
     y2_se[i] = sqrt(inflator * mu[y2_days[i]] * (1 - mu[y2_days[i]]) / y2_ss[i]); 
  }
  for(i in 1:y3_n){
     y3_se[i] = sqrt(inflator * mu[y3_days[i]] * (1 - mu[y3_days[i]]) / y3_ss[i]); 
  }
  for(i in 1:y4_n){
     y4_se[i] = sqrt(inflator * mu[y4_days[i]] * (1 - mu[y4_days[i]]) / y4_ss[i]); 
  }
  for(i in 1:y5_n){
     y5_se[i] = sqrt(inflator * mu[y5_days[i]] * (1 - mu[y5_days[i]]) / y5_ss[i]); 
  }


  // state model
  mu[1] ~ normal(mu_start, 0.0001); // starting point
  
  sigma ~ normal(0.005, 0.005);              // prior for innovation sd.  
  
  mu[2:n_days] ~ student_t(4, mu[1:(n_days - 1)], sigma);
  // mu[2:n_days] ~ normal(mu[1:(n_days - 1)], sigma);
      
  // measurement model
  // 1. Election result
  mu_finish ~ normal(mu[n_days], 0.0001);
  
  // 2. Polls
  d ~ normal(0, 0.05); // ie a fairly loose prior for house effects (on scale of [0,1])
  
  y1_values ~ normal(mu[y1_days] + d[1], y1_se);
  y2_values ~ normal(mu[y2_days] + d[2], y2_se);
  y3_values ~ normal(mu[y3_days] + d[3], y3_se);
  y4_values ~ normal(mu[y4_days] + d[4], y4_se);
  y5_values ~ normal(mu[y5_days] + d[5], y5_se);

}
