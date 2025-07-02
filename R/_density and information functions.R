
InfoBoundIRT=function(theta=theta,beta=beta,alpha=alpha,b0=b0,b1=b1,si2=si2,model){      #si2 is the second canonical parameter (omega, phi, or delta, depending on the model)
 if(model=="beta" | model=="beta_nobound"){
    a = exp(.5* (alpha*theta + beta+si2));
    b  = exp(.5* (-alpha*theta - beta+si2));
    out=-alpha*((trigamma(a+b)*((a-b)/2)^2-trigamma(a)*(a/2)^2-trigamma(b)*(b/2)^2))
 }

 if(model=="simplex"){
 infX2=function(x,theta,beta,alpha,si2){
    mu=plogis(alpha*theta+beta)
    tmp= ((x - mu) * (x * (2*mu - 1) - mu^2))/((x - 1) * x * si2 * (1 - mu)^3 * mu^3) * alpha * mu * (1-mu)
    return(tmp^2*dsimplex(x,mu,sqrt(si2)))
 }
 out=c()
 for(i in 1:length(theta)) out[i]=integrate(infX2,0.0001,.9999,theta=theta[i],beta=beta,alpha=alpha,si2=si2)$value
 }

 if(model=="samejima") out=rep(alpha^2/si2,length(theta))
 if(model=="normal") out=rep(alpha^2/si2,length(theta))

 return(out)
}


dBoundIRT=function(x,beta=beta,alpha=alpha,b0=b0,b1=b1,si2=si2,model){       #missing is coded as -999
 if(model=="beta"){                                                            #si2 is the second canonical parameter (omega, phi, or delta, depending on the model)
 marg=function(theta,x=x,beta=beta,alpha=alpha,b0=b0,b1=b1,si2=si2){
    if(x!=0 & x!=1 & x!=-999){
       a = exp(.5* (alpha*theta + beta+si2));
       b  = exp(.5* (-alpha*theta - beta+si2));
       out= (plogis(b1-alpha*theta) - plogis(b0-alpha*theta))*dbeta(x,a,b)
    }
    else if(x==0) out=plogis(b0-alpha*theta)
    else if(x==1) out=1-plogis(b1-alpha*theta)
  return(out*dnorm(theta))
  }
  }

 ## added by merkle: regular beta without bounds
 if(model=="beta_nobound"){
   marg <- function(theta, x = x, beta = beta, alpha = alpha, si2 = si2){
     a <- exp(.5* (alpha*theta + beta+si2))
     b <- exp(.5* (-alpha*theta - beta+si2))
     out <- dbeta(x, a, b)
     return(dbeta(x, a, b) * dnorm(theta))
   }
 }
 
 if(model=="simplex"){
 marg=function(theta,x=x,beta=beta,alpha=alpha,b0=b0,b1=b1,si2=si2){
    if(x!=0 & x!=1 & x!=-999) out= (plogis(b1-alpha*theta) - plogis(b0-alpha*theta))*dsimplex(x,plogis(beta+alpha*theta),sqrt(si2))
    else if(x==0) out=plogis(b0-alpha*theta)
    else if(x==1) out=1-plogis(b1-alpha*theta)
  return(out*dnorm(theta))
  }
  }

 if(model=="samejima"){
 marg=function(theta,x=x,beta=beta,alpha=alpha,b0=b0,b1=b1,si2=si2){
    if(x!=0 & x!=1 & x!=-999) out= (plogis(b1-alpha*theta) - plogis(b0-alpha*theta))*1/(x*(1-x))*dnorm(qlogis(x),beta+alpha*theta,sqrt(si2))
    else if(x==0) out=plogis(b0-alpha*theta)
    else if(x==1) out=1-plogis(b1-alpha*theta)
  return(out*dnorm(theta))
  }
  }
  ou <- rep(NA, length(x))
  for(i in 1:length(x)){
    ou[i]=integrate(marg,-Inf,Inf,x=x[i],beta=beta,alpha=alpha,b0=b0,b1=b1,si2=si2)$value
  }
 
 return(ou)
}

## spacings added by Merkle
InfoBoundIRTinf <- function(theta=theta,beta=beta,alpha=alpha,b0=b0,b1=b1,si2=si2,model){     # see appendix A from the paper
  ou <- rep(NA, length(theta))                                                                                       #si2 is the second canonical parameter (omega, phi, or delta, depending on the model)
  for(i in 1:length(theta)){
    ou[i] <- alpha^2 * plogis(-(b0 - alpha*theta[i]))^2 * plogis((b0 - alpha*theta[i])) +                     #info lower bound
      (plogis((b1 - alpha*theta[i])) - plogis((b0 - alpha*theta[i]))) *
      ((-alpha * (plogis((b1 - alpha*theta[i])) * plogis(-(b1 - alpha*theta[i])) -
                  plogis((b0 - alpha*theta[i])) * plogis(-(b0 - alpha*theta[i]))) /
                 (plogis((b1 - alpha*theta[i])) - plogis((b0 - alpha*theta[i]))))^2 +  #info 0<X<1
       InfoBoundIRT(theta = theta[i], beta = beta, alpha = alpha, b0 = b0, b1 = b1, si2 = si2, model)) +       # info traditional model
      alpha^2 * plogis((b1 - alpha*theta[i]))^2 * plogis(-(b1 - alpha*theta[i]))                       #info upper bound
  }
  return(ou)
}


InfoIRTinf <- function(theta=theta, beta=beta, alpha=alpha, si2=si2, model = "beta_nobound"){
  ou <- rep(NA, length(theta))

  for (i in 1:length(theta)) {
    ou[i] <- InfoBoundIRT(theta = theta[i], beta = beta, alpha = alpha, b0 = NULL, b1 = NULL, si2 = si2, model)
  }

  return(ou)
}


## obtain empirical bayes score based on a person's responses to a subset of items
scoreBoundIRT <- function(x, items, beta, alpha, b0, b1, si2) {

  ## optimization function
  f.eta.i <- function(theta, x, items, beta, alpha, b0, b1, si2){
    out <- rep(NA, length(x))

    for (i in 1:length(x)) {
      itnum <- items[i]
      if(x[i] != 0 & x[i] != 1 & x[i] != -999){
       a <- exp(.5* (alpha[itnum] * theta + beta[itnum] + si2[itnum]))
       b <- exp(.5* (-alpha[itnum] * theta - beta[itnum] + si2[itnum]))
       out[i] <- log(plogis(b1[itnum] - alpha[itnum] * theta) - plogis(b0[itnum] - alpha[itnum] * theta)) + dbeta(x[i], a, b, log = TRUE)
      } else if (x[i] == 0) {
        out[i] <- plogis(b0[itnum] - alpha[itnum] * theta, log.p = TRUE)
      } else if (x[i] == 1) {
        out[i] <- plogis(b1[itnum] - alpha[itnum] * theta, lower.tail = FALSE, log.p = TRUE)
      }
    }
    
    return(-sum(out) - dnorm(theta, log = TRUE))
  }

  out <- nlminb(0, objective = f.eta.i, gradient = NULL, control = list(rel.tol = 1e-8),
                x = x, items = items, beta = beta, alpha = alpha, b0 = b0, b1 = b1, si2 = si2)

  if (out$convergence == 0L) {
    eta.i <- out$par
  } else {
    eta.i <- NA
  }

  eta.i
}


## testing
if (FALSE) {
  library(targets) # may need setwd('../')
  library(rstan)
  source('R/_density and information functions.R')

  tar_load(data)
  tar_load(summaries)
  tar_load(postmns)
  tar_load(fit)
  tmpsum <- summary(fit)[[1]]

  nsub <- 160
  out <- matrix(NA, nsub, 2)
  for (i in 1:nsub) {
    obs <- which(data[i,] != -999)
    out[i, 1] <- scoreBoundIRT(data[i,obs], items = obs, postmns$beta, postmns$alpha, postmns$b0, postmns$b1, postmns$si2)
    out[i, 2] <- tmpsum[paste0('tau[', i, ']'), 'mean']
  }

  ## estimates match
  plot(out, pch = apply(data==-999,1,sum)); abline(0,1)

  ## NB we cannot easily use f.eta.i to obtain a numerical hessian that is comparable to the
  ## information function. This is because the information function does not depend on observed data.
  ## (p. 65 Baker & Kim: observed data are replaced by their expectations, because we are conditioning
  ##  on item parameters as "true")
  ## Below shows that our numerical hessians generally lead to larger SEs, as expected.
  library(numDeriv)
  
  ## optimization function
  f.eta.i <- function(theta, dat, items, beta, alpha, b0, b1, si2){
    out <- NA

    for (i in 1:length(dat)) {
      itnum <- items[i]
      if(dat[i] != 0 & dat[i] != 1 & dat[i] != -999){
        a <- exp(.5* (alpha[itnum] * theta + beta[itnum] + si2[itnum]))
        b <- exp(.5* (-alpha[itnum] * theta - beta[itnum] + si2[itnum]))
        out[i] <- log(plogis(b1[itnum] - alpha[itnum] * theta) - plogis(b0[itnum] - alpha[itnum] * theta)) + dbeta(dat[i], a, b, log = TRUE)
      } else if (dat[i] == 0) {
        out[i] <- plogis(b0[itnum] - alpha[itnum] * theta, log.p = TRUE)
      } else if (dat[i] == 1) {
        out[i] <- plogis(b1[itnum] - alpha[itnum] * theta, lower.tail = FALSE, log.p = TRUE)
      }
    }
    
    return(-sum(out))
  }


  compobs <- which(apply(data != -999, 1, sum) == 20)

  res <- matrix(NA, 80, 2)
  for (idx in 1:80){
    i <- compobs[idx]

    myhess <- hessian(f.eta.i, x = out[i,1], dat = data[i,], items = 1:20, beta = postmns$beta, alpha = postmns$alpha, b0 = postmns$b0, b1 = postmns$b1, si2 = postmns$si2)

    tinf <- rep(NA, 20)
    for (j in 1:20) {
      tinf[j] <- InfoBoundIRTinf(out[i,1], postmns$beta[j], postmns$alpha[j], postmns$b0[j], postmns$b1[j], postmns$si2[j], model = "beta")
    }
    res[idx,] <- c(myhess[1,1], sum(tinf))
  }

  ## compare standard errors
  plot(1 / sqrt(res), xlab = "Numerical Hessian", ylab = "Information function")
}
