data {
  int<lower=0> N;
  int<lower=0> nit;
  real z[N,nit];
} parameters {
  real tau[N];
  real icept[nit];
  real ln_sigma2[nit];
  real ln_phi[nit];
  real b0[nit];
  real b1[nit];
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
      if(z[p,i] != 0 && z[p,i] != 1 && z[p,i] != -999) {
        1 ~ bernoulli(1/(1+exp(-(b1[i]-phi[i]*tau[p])))-1/(1+exp(-(b0[i]-phi[i]*tau[p]))));
        z[p,i] ~ beta(alpha[p,i],beta[p,i]);      
      } else if(z[p,i] == 0) {
        1 ~ bernoulli(1/(1+exp(-(b0[i]-phi[i]*tau[p]))));       
      } else if(z[p,i] == 1) {
        1 ~ bernoulli(1-1/(1+exp(-(b1[i]-phi[i]*tau[p]))));
      }
    }
  }  

  b0 ~ normal(-2, 1); // cutpoint for lower bound vs middle
  b1 ~ normal(2, 1); // cutpoint for middle vs upper bound
  icept ~ normal(0, 2); // beta difficulty
  ln_sigma2 ~ normal(0,10); // beta variability
  ln_phi ~ normal(0,1); // beta discrimination
} generated quantities {
  real zrep[N,nit];
  
  for (p in 1:N) {
    for (i in 1:nit) {
      int mixout;
      vector[3] cprobs;

      cprobs[1] = inv_logit(b0[i]-phi[i]*tau[p]);
      cprobs[2] = inv_logit(b1[i]-phi[i]*tau[p]) - cprobs[1];
      cprobs[3] = 1 - cprobs[1] - cprobs[2];
      mixout = categorical_rng(cprobs);

      if (mixout == 1) {
        zrep[p, i] = 0.0;
      } else if (mixout == 2) {
        zrep[p, i] = beta_rng(alpha[p,i], beta[p,i]);
      } else {
        zrep[p, i] = 1.0;
      }
    }
  }
}

