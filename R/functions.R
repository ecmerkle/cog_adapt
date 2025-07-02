read_data <- function(bounds = TRUE) {

  dat <- read.csv("../fpt/data_pilot_cognitive_tasks/convenience_datasets/summary_scores_all_tasks_by_subject_id.csv")
  dat <- dat[,c(1:12, 18, 13:17, 19:21)]
  names(dat)[2:21] <- c('leapfrog', 'denominator_neglect_A', 'denominator_neglect_B', 'graph_literacy', 'percent_correct_GK', 'percent_correct_IQ', 'calibration_GK', 'times_series_score', 'bayesian_update_easy', 'bayesian_update_hard', 'cognitive_reflecton', 'berlin_numeracy', 'number_series', 'cfs', 'raven', 'shipley_vocab', 'shipley_abstraction', 'ADMC_resistance_to_framing', 'ADMC_risk_perception', 'ADMC_decision_rules')

  dat$calibration_GK <- -dat$calibration_GK
  dat$bayesian_update_easy <- -dat$bayesian_update_easy
  dat$bayesian_update_hard <- -dat$bayesian_update_hard
  dat$ADMC_resistance_to_framing <- -dat$ADMC_resistance_to_framing
  dat$times_series_score <- -dat$times_series_score

  ## to exclude wave 0
  fdat <- read.csv("../fpt/data_pilot_forecasting/processed_data/pilot scores.csv")
  goodsub <- unique(fdat$subject_id[fdat$wave != 0])
  dat <- subset(dat, subject_id %in% goodsub)
  pilotsubs <- unique(dat$subject_id)
  
  lbound <- apply(dat[,2:21], 2, min, na.rm = TRUE)
  ubound <- apply(dat[,2:21], 2, max, na.rm = TRUE)
  
  scdat <- t(apply(dat[,2:21], 1, function(x) (x - lbound)/(ubound - lbound)))

  if (!bounds) {
    scdat[scdat <= .0001] <- .0001
    scdat[scdat >= .9999] <- .9999
  }
  
  scdat[is.na(scdat)] <- -999

  list(data = scdat, pilotsubs = pilotsubs, lbound = lbound, ubound = ubound)
}


read_data2 <- function(bounds = TRUE, lbound = NULL, ubound = NULL) {
  dat2 <- read.csv('../fpt/data_cognitive_tasks/convenience_datasets/summary_scores_all_tasks_by_subject_id.csv')
  dat2 <- dat2[,c(1:12, 18, 13:17, 19:21)]
  names(dat2)[2:21] <- c('leapfrog', 'denominator_neglect_A', 'denominator_neglect_B', 'graph_literacy', 'percent_correct_GK', 'percent_correct_IQ', 'calibration_GK', 'times_series_score', 'bayesian_update_easy', 'bayesian_update_hard', 'cognitive_reflecton', 'berlin_numeracy', 'number_series', 'cfs', 'raven', 'shipley_vocab', 'shipley_abstraction', 'ADMC_resistance_to_framing', 'ADMC_risk_perception', 'ADMC_decision_rules')

  dat2$calibration_GK <- -dat2$calibration_GK
  dat2$bayesian_update_easy <- -dat2$bayesian_update_easy
  dat2$bayesian_update_hard <- -dat2$bayesian_update_hard
  dat2$ADMC_resistance_to_framing <- -dat2$ADMC_resistance_to_framing
  dat2$times_series_score <- -dat2$times_series_score

  scdat2 <- t(apply(dat2[,2:21], 1, function(x) (x - lbound)/(ubound - lbound)))

  scdat2[scdat2 < 0] <- 0
  scdat2[scdat2 > 1] <- 1

  if (!bounds) {
    scdat2[scdat2 <= .0001] <- .0001
    scdat2[scdat2 >= .9999] <- .9999
  }

  scdat2[is.na(scdat2)] <- -999

  
  ## obtain forecast scores
  fdat <- read.csv("../fpt/data_forecasting/processed_data/scores_quantile.csv")
  mnscore <- with(fdat, tapply(sscore_standardized, subject_id, mean))

  mnscore <- mnscore[names(mnscore) %in% dat2$subject_id]
  mnscore[match(names(mnscore), dat2$subject_id)] <- mnscore

  list(dat = scdat2, smad = mnscore)
}


read_data3 <- function(data2) {
  ## add forecast items to full study cognitive scores
  fdat <- read.csv("../fpt/data_forecasting/processed_data/scores_quantile.csv")
  items <- unique(fdat$item)

  ## for subject ids
  dat2 <- read.csv('../fpt/data_cognitive_tasks/convenience_datasets/summary_scores_all_tasks_by_subject_id.csv')
  fdat <- subset(fdat, subject_id %in% dat2$subject_id)

  subs <- unique(fdat$subject_id)
  fmat <- matrix(NA, length(subs), length(items))

  for (i in 1:length(subs)) {
    tmpscore <- fdat$sscore_standardized[fdat$subject_id == subs[i]]
    tmpitem <- fdat$item[fdat$subject_id == subs[i]]

    fmat[i, match(tmpitem, items)] <- (tmpscore + 4)/(5 + 4)
  }

  fmat[match(subs, dat2$subject_id), ] <- fmat
  colnames(fmat) <- items

  out <- cbind(data2, 1 - fmat) # we reverse fmat here so that larger values are always better
  out[is.na(out)] <- -999

  out
}

  
  

read_times <- function() {
  ## return median times for now, could change later
  ## pcorrGK + calGK are unknown, so use pcorrIQ for now
  tms <- c(leap = 7.92, denomA = 3.95, denomB = 3.17, graph = 3.99, pcorrGK = 5.38, pcorrIQ = 5.38, calGK = 5.38,
           time_ser = 5.64, bayes_easy = 6.63, bayes_hard = 6.20, cog_refl = 2.41, berlin = 1.56,
           num_ser = 3.39, cfs = 3.35, raven = 10.97, ship_voc = 2.77, ship_abs = 9.81,
           ## these come from datasets_to_save$median_completion_times:
           ADMC_res = 8.06, ADMC_risk = 2.09, ADMC_dec = 5.30)

  sclabs <- c("leap", "denomA", "denomB", "graph", "pcorrGK", "pcorrIQ", "calGK", "time_ser",
              "bayes_easy", "bayes_hard", "cog_refl", "berlin", "num_ser", "cfs", "raven",
              "ship_voc", "ship_abs", "ADMC_res", "ADMC_risk", "ADMC_dec")
  names(sclabs) <- 1:20

  list(tms = tms, sclabs = sclabs)
}

read_times3 <- function(data3) {
  ## placeholder to make adaptive testing functions happy
  tms <- rep(1, ncol(data3))
  names(tms) <- colnames(data3)

  sclabs <- colnames(data3)
  names(sclabs) <- 1:ncol(data3)

  list(tms = tms, sclabs = sclabs)
}
  

read_fscores <- function(pilotsubs) {
  ## get forecast scores, arrange in same order as pretest data
  fdat <- read.csv("../fpt/data_pilot_forecasting/processed_data/pilot scores.csv")

  ## exclude wave 0
  fdat <- subset(fdat, wave != 0)
  
  mnscore <- with(fdat, tapply(sscore_standardized, subject_id, mean))

  ## keep only those who are in cognitive data
  mnscore <- mnscore[names(mnscore) %in% pilotsubs]
  out <- cbind.data.frame(id = pilotsubs, mnscore = rep(NA, length(pilotsubs)))
  out$mnscore[match(names(mnscore), pilotsubs)] <- mnscore

  out
}

fit_model <- function(scdat, model, model_type = "beta") {
  N <- nrow(scdat)
  nit <- ncol(scdat)

  ini <- list(icept = rep(0, nit), ln_sigma2 = rep(0, nit),
              ln_phi = rep(0, nit), tau = rep(0, N))
  mons <- c("icept", "phi", "ln_sigma2", "zrep", "tau")
  if (model_type != "beta_nobound") {
    ini <- c(ini, list(b0 = rep(-2, nit), b1 = rep(2, nit)))
    mons <- c(mons, "b0", "b1")
  }

  ini <- list(c1 = ini, c2 = ini, c3 = ini)

  standata <- list(N = N, nit = nit, z = scdat)

  fit <- sampling(model, data = standata, iter = 2000, chains = 3, init = ini,
                  pars = mons)

  fit
}

summ_params <- function(fit) {
  ## obtain posterior means of model parameters
  summ <- summary(fit)[[1]]

  beta <- summ[grepl('icept', rownames(summ)), 'mean']
  alpha <- summ[grepl('^phi', rownames(summ)), 'mean']
  b0 <- summ[grepl('b0', rownames(summ)), 'mean']
  b1 <- summ[grepl('b1', rownames(summ)), 'mean']
  si2 <- summ[grepl('ln_sigma2', rownames(summ)), 'mean']

  betasd <- summ[grepl('icept', rownames(summ)), 'sd']
  alphasd <- summ[grepl('^phi', rownames(summ)), 'sd']
  b0sd <- summ[grepl('b0', rownames(summ)), 'sd']
  b1sd <- summ[grepl('b1', rownames(summ)), 'sd']
  si2sd <- summ[grepl('ln_sigma2', rownames(summ)), 'sd']
  
  list(beta = beta, alpha = alpha, b0 = b0, b1 = b1, si2 = si2,
       betasd = betasd, alphasd = alphasd, b0sd = b0sd, b1sd = b1sd, si2sd = si2sd)
}


summ_outputs <- function(scdat, fit, thetseq, postmns, model_type = "beta") {
  ## posterior mean information functions

  nit <- length(postmns$beta)
  infuns <- matrix(NA, length(thetseq), nit)

  if (model_type == "beta_nobound") {
    for (i in 1:nit) {
      infuns[,i] <- with(postmns, InfoIRTinf(thetseq, beta[i], alpha[i], si2[i], model = 'beta_nobound'))
    }
  } else {    
    for (i in 1:nit) {
      infuns[,i] <- with(postmns, InfoBoundIRTinf(thetseq, beta[i], alpha[i], b0[i], b1[i], si2[i], model = 'beta'))
    }
  }

  longthet <- cbind.data.frame(info = as.numeric(infuns), theta = rep(thetseq, nit),
                               item = rep(1:nit, each = length(thetseq)))
  
  ## posterior sample information functions
  draws <- as.data.frame(fit)
  
  nsamp <- nrow(draws)
  preds <- vector("list", nrow(draws))

  for (i in 1:nrow(draws)) {
    beta <- draws[i, grepl('icept', colnames(draws))]
    alpha <- draws[i, grepl('^phi', colnames(draws))]
    b0 <- draws[i, grepl('b0', colnames(draws))]
    b1 <- draws[i, grepl('b1', colnames(draws))]
    si2 <- draws[i, grepl('ln_sigma2', colnames(draws))]

    preds[[i]] <- matrix(NA, length(thetseq), nit)

    if (model_type == "beta_nobound") {
      for (j in 1:nit) {
        preds[[i]][,j] <- InfoIRTinf(thetseq, beta[[j]], alpha[[j]], si2[[j]], model = model_type)
      }
    } else {
      for (j in 1:nit) {
        preds[[i]][,j] <- InfoBoundIRTinf(thetseq, beta[[j]], alpha[[j]], b0[[j]], b1[[j]], si2[[j]], model = 'beta')
      }
    }
  }

  it1 <- sapply(preds, function(x) x[,1])

  allits <- as.numeric(sapply(preds, as.numeric))

  reps <- cbind.data.frame(info = allits, theta = rep(thetseq, nit * nsamp), iter = rep(1:nsamp, each = length(thetseq) * nit), item = rep(rep(1:nit, each = length(thetseq)), nsamp))

  ## posterior predictions of data + person parameters
  zrep <- draws[, grepl('zrep', colnames(draws))]
  tau <- draws[, grepl('tau', colnames(draws))]
  
  list(longthet = longthet, reps = reps, zrep = zrep, tau = tau)
}

score_study2 <- function(dat2, dat1, postmns) {
  ## scores of people from study 2, using study 1 parameters (or vice versa)
  s1names <- colnames(dat1)
  s2names <- colnames(dat2)

  ovsc <- rep(NA, nrow(dat2))

  for (i in 1:nrow(dat2)) {
    obs <- which(dat2[i,] != -999 & (s2names %in% s1names))

    ovsc[i] <- scoreBoundIRT(dat2[i, obs], items = match(s2names[obs], s1names),
                             postmns$beta, postmns$alpha, postmns$b0, postmns$b1, postmns$si2)
  }

  ovsc
}
