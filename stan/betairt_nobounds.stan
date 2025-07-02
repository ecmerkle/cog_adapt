data {
  int<lower=0> N;
  int<lower=0> nit;
  real z[N,nit];
} parameters {
  real tau[N];
  real icept[nit];
  real ln_sigma2[nit];
  real ln_phi[nit];
} transformed parameters{
  real<lower=0> phi[nit];
  real<lower=0> alpha[N,nit];
  real<lower=0> beta[N,nit];

  for(i in 1:nit){
    phi = exp(ln_phi);
    
    for(p in 1:N){
      alpha[p,i] = exp(.5* (phi[i]*tau[p] + icept[i] + ln_sigma2[i])); // paper, eq 4
      beta[p,i] = exp(.5* (-phi[i]*tau[p] - icept[i] + ln_sigma2[i])); // paper, eq 5
    }
  }
} model {
  for(p in 1:N) {
    tau[p] ~ normal(0,1);

    for(i in 1:nit) {
      if (z[p,i] != -999) {
	z[p,i] ~ beta(alpha[p,i],beta[p,i]);
      }
    }
  }  

  icept ~ normal(0, 2); // beta difficulty
  ln_sigma2 ~ normal(0,10); // beta variability
  ln_phi ~ normal(0,1); // beta discrimination
} generated quantities {
  real zrep[N,nit];
  
  for (p in 1:N) {
    for (i in 1:nit) {
      zrep[p, i] = beta_rng(alpha[p,i], beta[p,i]);
    }
  }
}

