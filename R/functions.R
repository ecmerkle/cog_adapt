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

  
  

read_times <- function(data) {
  ## return median times for now, could change later
  ## pcorrGK + calGK are unknown, so use pcorrIQ for now
  tms <- c(leap = 7.92, denomA = 3.95, denomB = 3.17, graph = 3.99, pcorrGK = 5.38, pcorrIQ = 5.38, calGK = 5.38,
           time_ser = 5.64, bayes_easy = 6.63, bayes_hard = 6.20, cog_refl = 2.41, berlin = 1.56,
           num_ser = 3.39, cfs = 3.35, raven = 10.97, ship_voc = 2.77, ship_abs = 9.81,
           ## these come from datasets_to_save$median_completion_times:
           ADMC_res = 8.06, ADMC_risk = 2.09, ADMC_dec = 5.30)

  names(tms) <- sclabs <- colnames(data)
  names(sclabs) <- 1:20

  list(tms = tms, sclabs = sclabs)
}

read_times3 <- function(data3) {
  ## get times from cognitive tests, then add the forecasting times
  tms <- read_times(data3[,1:20])
  ftimes <- read.csv("data/item rt descriptives.csv")

  ftimes <- ftimes[match(colnames(data3)[21:ncol(data3)], ftimes$item),]
  newnames <- colnames(data3)

  tms <- c(tms$tms, ftimes$median)
  ## 6 forecasting items are missing time taken, so use median across other forecasting items:
  tms[is.na(tms)] <- median(ftimes$median, na.rm = TRUE)
  names(tms) <- newnames
  
  sclabs <- names(tms)
  names(sclabs) <- 1:length(sclabs)

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

fit_2grp_model <- function(scdat1, scdat2, model) {
  scdat <- rbind(scdat1, scdat2)
  N <- nrow(scdat)
  nit <- ncol(scdat)
  grp <- rep(1:2, c(nrow(scdat1), nrow(scdat2)))

  ini <- list(icept = array(0, dim = c(2, nit)), ln_sigma2 = array(0, dim = c(2, nit)),
              ln_phi = array(0, dim = c(2, nit)), tau = array(0, dim = N),
              b0 = array(-2, dim = c(2, nit)), b1 = array(2, dim = c(2, nit)))
  ini <- list(c1 = ini, c2 = ini, c3 = ini)
  
  mons <- c("icept", "phi", "ln_sigma2", "b0", "b1")
  mons <- c(mons, paste0(mons, "_diff"))
  standata<- list(N = N, nit = nit, z = scdat, grp = grp)

  fit <- sampling(model, data = standata, iter = 2000, chains = 3, init = ini, pars = mons)

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

read_forecasts <- function() {
  ## read study 1 forecasts for aggregation section
  fdat <- read.csv("../fpt/data_pilot_forecasting/processed_data/pilot scores.csv")

  ## analyses of scores on overlapping questions vs across all questions
  ## tmpdat <- fdat[fdat$item %in% c('G1472', 'G1588', 'G1920', 'M3028', 'M3701', 'M5839'),]
  ## olscore <- with(tmpdat, tapply(sscore_standardized, subject_id, mean, na.rm = TRUE))
  ## fullscore <- with(fdat, tapply(sscore_standardized, subject_id, mean, na.rm = TRUE))
  ## fullscore <- fullscore[names(fullscore) %in% names(olscore)]
  ## cor(olscore, fullscore) # .89
  ## plot(olscore, fullscore)
  
  fdat
}

## aggregations using results of adaptive test
do_agg <- function(data, fcasts, adapt_test, pctkeep = .4) {
  pilotsubs <- data$pilotsubs
  data <- data$data
  compdat <- which(apply(data == -999, 1, sum) == 0)
  subids <- pilotsubs[compdat]
  
  ## forecasts nonnegative and monotonic across quantiles
  valids <- apply(fcasts, 1, function(x) all(diff(as.numeric(x[paste0("X", 1:5)])) >= 0)) &
    apply(fcasts, 1, function(x) all(as.numeric(x[paste0("X", 1:5)]) > 0))
  gtruth <- with(fcasts, tapply(resolution, item, head, 1))
  omag <- 10^floor(log(gtruth, base = 10))

  ## full aggregation + score
  fullagg <- exp(apply(fcasts[valids, paste0('X', 1:5)], 2, function(x) tapply(log(x), fcasts$item[valids], mean, na.rm = TRUE)))
  ssc <- sscore(c(.05, .25, .5, .75, .95), apply(fullagg, 2, function(x) x/omag), gtruth/omag)

  ## select pctkeep at each step of adaptive test
  nkeep <- round(pctkeep * length(subids))

  selscs <- intwidth <- matrix(NA, 18, nrow(ssc))
  aggnow <- vector("list", 18)
  for (i in 1:18) {
    scoresnow <- sapply(adapt_test, function(x) x$score[i])
  
    keepnow <- tail((1:146)[order(scoresnow)], nkeep)

    tmpdat <- fcasts[fcasts$subject_id %in% subids[keepnow],]
    valids <- apply(tmpdat, 1, function(x) all(diff(as.numeric(x[paste0("X", 1:5)])) >= 0)) &
      apply(tmpdat, 1, function(x) all(as.numeric(x[paste0("X", 1:5)]) > 0))

    aggnow[[i]] <- exp(apply(tmpdat[valids, paste0('X', 1:5)], 2, function(x) tapply(log(x), tmpdat$item[valids], mean, na.rm = TRUE)))
    scnow <- sscore(c(.05, .25, .5, .75, .95), apply(aggnow[[i]], 2, function(x) x/omag), gtruth/omag)

    selscs[i,] <- scnow$total_score
    intwidth[i,] <- (aggnow[[i]][,'X5'] - aggnow[[i]][,'X1'])/gtruth
  }

  allscs <- data.frame(score = c(ssc$total_score, as.numeric(t(selscs))),
                       width = c(log(fullagg[,'X5'] - fullagg[,'X1']), as.numeric(t(intwidth))),
                       step = rep(0:18, each = nrow(ssc)), item = rep(1:nrow(ssc), 19))

  improv <- with(allscs, tapply(score, item, function(x) x))
  improv <- do.call("rbind", improv)
  impdf <- data.frame(improv = as.numeric(improv), step = rep(0:18, each = nrow(improv)), item = rep(1:nrow(improv), ncol(improv)))

  ## keep aggregated judgments at steps 0, 1, 18
  aggdat <- data.frame(judgment = as.numeric(as.matrix(apply(fullagg, 2, function(x) x/omag))), quantile = c(.05, .25, .5, .75, .95),
                       item = rep(rownames(fullagg), 5), truth = rep(gtruth/omag, 5))
  aggdat1 <- data.frame(judgment = as.numeric(as.matrix(apply(aggnow[[1]], 2, function(x) x/omag))), quantile = c(.05, .25, .5, .75, .95),
                        item = rep(rownames(fullagg), 5), truth = rep(gtruth/omag, 5))
  aggdat2 <- data.frame(judgment = as.numeric(as.matrix(apply(aggnow[[18]], 2, function(x) x/omag))), quantile = c(.05, .25, .5, .75, .95),
                        item = rep(rownames(fullagg), 5), truth = rep(gtruth/omag, 5))
  
  aggcomp <- rbind(aggdat, aggdat1, aggdat2)
  aggcomp$agg <- rep(paste0("Step ", c(0, 1, 18)), each = nrow(aggdat))

  list(impdf = impdf, aggcomp = aggcomp)
}


# function for calculating the s-score
# the first argument is a vector with the quantiles that respondents were asked to forecast about
# the second argument is a matrix with each column a quantile and each row a response that has a forecast for the respective quantile
# the third argument is a vector that indicates the correct answer corresponding to each response
sscore <- function(ps, qs, tr){
    scrs <- matrix(NA, nrow = nrow(qs), ncol = ncol(qs))
    for(i in 1:nrow(scrs)){
        for(j in 1:ncol(scrs)){
            scrs[i, j] <- ps[j] * max(tr[i] - qs[i,j], 0) + (1 - ps[j]) * max(qs[i,j] - tr[i], 0)
        }
    }
    scrs <- data.frame(scrs)
    names(scrs) <- ps
    scrs$total_score <- rowSums(scrs[, 1:ncol(qs)])
    return(scrs)
}
