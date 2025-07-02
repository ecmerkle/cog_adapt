## backcast an adaptive testing routine on existing data
## dat: a data matrix of pretest scores
## itemparms: estimated item parameters, used for computing information functions
## time: vector of median time taken per test, for information/minute
run_adaptive <- function(dat, itemparms, time = NULL) {
  ntest <- ncol(dat)
  stopifnot(ntest == length(itemparms$beta))

  if (length(time) == 0) time <- rep(1, ntest)
  
  ## create lookup table of test information at set values of theta
  thetas <- seq(-3, 3, .01)
  lookuptab <- matrix(NA, ntest, length(thetas))

  for (j in 1:ntest) {
    lookuptab[j,] <- InfoBoundIRTinf(thetas, beta = itemparms$beta[j], alpha = itemparms$alpha[j],
                                     b0 = itemparms$b0[j], b1 = itemparms$b1[j],
                                     si2 = itemparms$si2[j], model = "beta") / time[j]
  }
  ## rank of items for each value of theta (order in which they should be administered)
  lookupranks <- apply(lookuptab, 2, rank)

  ## now run backcasting on each row of dat
  out <- vector("list", nrow(dat))
  start_item <- which.max(lookuptab[,thetas == 0])

  for (i in 1:nrow(dat)) {
    ## reset test tracker
    seen_tests <- rep(NA, ntest)
    seen_tests[1] <- start_item
    run_info <- run_score <- rep(NA, ntest)

    for (j in 1:ntest) {
      ## score the individual based on items seen so far
      scord <- seen_tests[1:j][order(seen_tests[1:j])]
      run_score[j] <- scoreBoundIRT(dat[i, scord], items = scord, itemparms$beta,
                                    itemparms$alpha, itemparms$b0, itemparms$b1, itemparms$si2)

      ## choose the row of lookuptab that is closest to the estimated score
      tmpdif <- abs(run_score[j] - thetas)
      tabrow <- which(tmpdif == min(tmpdif))
      ## for ties, choose the least extreme theta
      if (length(tabrow) > 1) {
        tabrow <- tabrow[which.min(abs(thetas[tabrow]))]
      }

      ## record running information based on tests seen so far
      run_info[j] <- sum(lookuptab[seen_tests[1:j], tabrow])
      
      ## choose the unseen test in this row with most information
      if (j < ntest) {
        untests <- (1:ntest)[-seen_tests[1:j]]
        newtest <- untests[which.max(lookupranks[untests, tabrow])]

        seen_tests[(j + 1)] <- newtest
      }
    }

    out[[i]] <- list(seen_tests = seen_tests, info = run_info, score = run_score)
  }

  out
}

## run adaptive test on complete data from study 1, using item estimates from study 2
adapt12 <- function(data, data2, postmns, median_times, use_times = FALSE) {
  if (inherits(data2, "list")) data2 <- data2$dat

  ## exclude calGK and pcorrIQ because they also come from the IQ test
  badtest <- which(colnames(data2) %in% c("calibration_GK", "percent_correct_IQ"))
  data2 <- data2[, -badtest]
  postmns <- lapply(postmns, function(x) x[-badtest])
  
  compdat <- data[apply(data != -999, 1, sum) == ncol(data),
                  match(colnames(data2), colnames(data), nomatch = 0)]
  
  algtimes <- mtimes <- median_times$tms[match(colnames(data2), colnames(data), nomatch = 0)]
  if (!use_times) algtimes <- rep(1, ncol(data2))

  out <- run_adaptive(compdat, postmns, algtimes)

  ## compute time taken
  runtime <- lapply(out, function(x) list(runtime = cumsum(mtimes[x$seen_tests])))

  out <- mapply(c, out, runtime, SIMPLIFY = FALSE)

  out
}


if (FALSE) {
  setwd('../')
  library(targets)
  source("R/functions.R")
  source("R/_density and information functions.R")
  source("R/adaptive.R")

  tar_load(data)
  tar_load(data2)
  tar_load(postmns2)

  ## complete data only
  dat <- data[apply(data != -999, 1, sum) == 20,]
  ## use only tests from the second dataset
  dat <- dat[, match(colnames(dat), colnames(data2), nomatch = 0)]

  res <- run_adaptive(dat, postmns2)

  testseq <- sapply(res, function(x) x$seen_tests)
  plot(1:17, testseq[,1], type = "l", xlab = "Tests Administered", ylab = "Test")
  for (i in 2:ncol(testseq)) lines(1:17, testseq[,i])
  table(apply(testseq, 2, paste, sep = ",", collapse = ""))

  testinf <- sapply(res, function(x) x$info)
  plot(1:17, 1/sqrt(testinf[,1]), type = "l", xlab = "Tests Administered", ylab = "SE", ylim = c(0, .8))
  for (i in 2:ncol(testinf)) lines(1:17, 1/sqrt(testinf[,i]))

  testsc <- sapply(res, function(x) x$score)
  plot(1:17, testsc[,1], type = "l", xlab = "Tests Administered", ylab = "Score", ylim = c(-3, 4))
  for (i in 2:ncol(testsc)) lines(1:17, testsc[,i])
}
