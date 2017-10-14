
data {
  int n;   // number of six-monthly observations
  int deaths[n];  // deaths per billion vehicle km travelled, six month period
}

parameters {
  real lambda;
  
  
}

model {
  lambda ~normal(5, 5);
  deaths ~ poisson(lambda);
}
