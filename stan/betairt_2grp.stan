data {
  int<lower=0> N; // total N across the two groups
  int<lower=0> nit;
  array[N] int<lower=1,upper=2> grp; // group indicator (1,2)
  real z[N,nit];
} parameters {
  vector[N] tau;
  array[2] vector[nit] icept;
  array[2] vector[nit] ln_sigma2;
  array[2] vector[nit] ln_phi;
  array[2] vector[nit] b0;
  array[2] vector[nit] b1;
} transformed parameters{
  array[2] vector[nit] phi;
  real<lower=0> alpha[N,nit];
  real<lower=0> beta[N,nit];

  phi = exp(ln_phi);  
  for(i in 1:nit){    
    for(p in 1:N){
      alpha[p,i] = exp(.5* (phi[grp[p], i]*tau[p] + icept[grp[p], i] + ln_sigma2[grp[p], i])); // paper, eq 4
      beta[p,i] = exp(.5* (-phi[grp[p], i]*tau[p] - icept[grp[p], i] + ln_sigma2[grp[p], i])); // paper, eq 5
    }
  }
} model {
  for(p in 1:N) {
    tau[p] ~ normal(0,1);

    for(i in 1:nit) {
      if(z[p,i] != 0 && z[p,i] != 1 && z[p,i] != -999) {
        1 ~ bernoulli(1/(1+exp(-(b1[grp[p], i] - phi[grp[p], i] * tau[p]))) - 1/(1+exp(-(b0[grp[p], i] - phi[grp[p], i]*tau[p]))));
        z[p,i] ~ beta(alpha[p,i], beta[p,i]);      
      } else if(z[p,i] == 0) {
        1 ~ bernoulli(1/(1+exp(-(b0[grp[p], i] - phi[grp[p], i] * tau[p]))));       
      } else if(z[p,i] == 1) {
        1 ~ bernoulli(1 - 1/(1+exp(-(b1[grp[p], i] - phi[grp[p], i] * tau[p]))));
      }
    }
  }  

  for (g in 1:2) {
    b0[g] ~ normal(-2, 1); // cutpoint for lower bound vs middle
    b1[g] ~ normal(2, 1); // cutpoint for middle vs upper bound
    icept[g] ~ normal(0, 2); // beta difficulty
    ln_sigma2[g] ~ normal(0,10); // beta variability
    ln_phi[g] ~ normal(0,1); // beta discrimination
  }
} generated quantities {
  vector[nit] icept_diff;
  vector[nit] ln_sigma2_diff;
  vector[nit] phi_diff;
  vector[nit] b0_diff;
  vector[nit] b1_diff;

  icept_diff = icept[2] - icept[1];
  ln_sigma2_diff = ln_sigma2[2] - ln_sigma2[1];
  phi_diff = phi[2] - phi[1];
  b0_diff = b0[2] - b0[1];
  b1_diff = b1[2] - b1[1];
}

