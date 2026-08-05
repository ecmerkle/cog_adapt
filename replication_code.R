## Step 1: Clone the github repos in the same parent folder.
## Run in terminal/command line:
## git clone git@github.com:forecastingresearch/fpt.git
## git clone git@github.com:ecmerkle/cog_adapt.git

## Now go to the cog_adapt folder and carry out remaining steps there.

## Step 2: Ensure that necessary packages are installed. The following
## R comand will attempt to install missing packages for you:
## source("pkgcheck.R")

## Step 3: Run code to fit models and etc. This will take awhile.
## In R, run:
## source("compile.R")

## Step 4: The commands below will now replicate results from the paper,
## using the results of Step 3.



## ----preliminaries, echo=FALSE, message=FALSE, warning=FALSE------------------
library("knitr")
## global knitr options
## opts_chunk$set(echo=FALSE, fig.path='figure/',fig.align='center',fig.show='hold',size='footnotesize', cache.path="cache/", warning=FALSE, message = FALSE, dev="png", dpi=300, fig.height=7, fig.width=7)

library("ggplot2")
library("GGally")
library("rstan")
library("bayesplot")

## load results run outside this document:
library(targets)
bounds <- TRUE

tar_load(median_times)
sclabs <- median_times$sclabs
median_times <- median_times$tms
tar_load(fcast_scores)
tar_load(postmns)
if (bounds) {
  tar_load(data)
  tar_load(summaries)
} else {
  tar_load(data_nb)
  tar_load(summaries_nb)
  data <- data_nb
  summaries <- summaries_nb
}
pilotsubs <- data$pilotsubs
data <- data$data

tar_load(postmns2)
tar_load(data2)
smad2 <- data2$smad
data2 <- data2$dat
tar_load(summaries2)
tar_load(scores21)
tar_load(adapt_test)
tar_load(adapt_test_time)
tar_load(fit2grp)
tar_load(aggres)
tar_load(adapt_test_time_rev)

fcast_its <- read.csv("forecast_itnames.csv")

## add person proficiency + sum score to forecast scores
fcast_scores$profmn <- with(summaries, apply(tau, 2, mean))
tmpdat <- data
tmpdat[tmpdat == -999] <- NA
fcast_scores$meansc <- rowMeans(tmpdat, na.rm = TRUE)


scprint <- gsub("_", "\\\\_", sclabs)


##############
## Figure 1
reps <- summaries$reps
set.seed(100)
itersamp <- sample(unique(reps$iter), 200)

zrep <- summaries$zrep

## impute missing to get this function to work
impdat <- data
for (i in 1:ncol(data)) {
  nas <- which(data[,i] == -999)
  if(length(nas) > 0) {
    impdat[nas,i] <- sample(data[-nas,i], length(nas))
  }
}

ppc_dens_overlay_grouped(y = as.numeric(impdat), yrep = as.matrix(zrep[itersamp,]), group = rep(sclabs, each = nrow(data)), adjust = 1.5) + theme(legend.position = "none") + xlab("Test score") + ylab("Density") + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), strip.text = element_text(size = 7))


##############
## Figure 2
ggplot(fcast_scores, aes(x = profmn, y = mnscore)) + geom_point() + ylab("Forecasting S-score") + xlab("Test proficiency (z-score)") + theme_linedraw()


##############
## Figure 3
longthet <- summaries$longthet
longthet$iter <- 1

ggplot(subset(reps, iter %in% itersamp), aes(x = theta, y = info, group = iter)) + geom_line(alpha = 0.4, colour = '#b3cde0') + geom_line(data = longthet, aes(x = theta, y = info), linewidth = 1.1) + facet_wrap( ~ item, labeller = labeller(item = sclabs)) + xlab("Test proficiency (z-score)") + ylab("Test information") + theme(strip.text = element_text(size = 7))


##############
## Figure 4
longthet <- summaries$longthet
longthet$iter <- 1
longthet$tms <- median_times[match(longthet$item, as.numeric(names(sclabs)))]
reps$tms <- median_times[match(reps$item, as.numeric(names(sclabs)))]


ggplot(subset(reps, iter %in% itersamp), aes(x = theta, y = info/tms, group = iter)) + geom_line(alpha = 0.4, colour = '#b3cde0') + geom_line(data = longthet, aes(x = theta, y = info/tms), linewidth = 1.1) + facet_wrap( ~ item, labeller = labeller(item = sclabs)) + xlab("Test proficiency (z-score)") + ylab("Test information per minute") + theme(strip.text = element_text(size = 7))


##############
## Results selecting subsets of cognitive tests
sub1 <- c("cognitive_reflecton", "denominator_neglect_A", "denominator_neglect_B", "ADMC_risk_perception", "shipley_vocab", "number_series")
colsel <- which(sclabs %in% sub1)
fcast_scores$subsc <- rowMeans(tmpdat[, colsel], na.rm = TRUE)
top50 <- cor(fcast_scores[fcast_scores[,2] < quantile(fcast_scores[,2], .5, na.rm=TRUE), 2:5], use='pair')
bot50 <- cor(fcast_scores[fcast_scores[,2] > quantile(fcast_scores[,2], .5, na.rm=TRUE), 2:5], use='pair')
all50 <- cor(fcast_scores[, 2:5], use='pair')

sub2 <- c(sub1, "cfs", "ADMC_decision_rules")
colsel2 <- which(sclabs %in% sub2)
fcast_scores$subsc2 <- rowMeans(tmpdat[, colsel2], na.rm = TRUE)
all502 <- cor(fcast_scores[, c(2:4,6)], use = 'pair')

subtime <- sum(median_times[names(median_times) %in% sub1])
subtime2 <- sum(median_times[names(median_times) %in% sub2])


##############
## Figure 5
ggpairs(fcast_scores[,c(2,4,5)],
        columnLabels = c("Forecasting S-score", "Mean of all cognitive tests", "Mean of subset of cognitive tests"),
        upper = list(continuous = GGally::wrap(ggally_cor, stars = F)))


##############
## Figure 6
parres <- summary(fit2grp)[[1]]
parres <- as.data.frame(parres[grep("diff", rownames(parres)), c('mean', '2.5%', '97.5%')])
parres$parm <- rep(c("beta", "alpha", "omega", "gamma1", "gamma2"), each = 20)
parres$test <- rep(sclabs, length(unique(parres$parm)))
names(parres)[2:3] <- c("lowpi", "hipi")
parres$inint <- 1 - with(parres, hipi < 0 | lowpi > 0) 


## ----parcomp------------------------------------------------------------------
itcols <- 1:20
itnames <- c("beta", "alpha", "gamma1", "gamma2", "omega")
stdat <- cbind.data.frame(parm = rep(itnames, each = length(itcols)),
                          st1mean = as.numeric(sapply(postmns[1:5], function(x) x[itcols])),
                          st1sd = as.numeric(sapply(postmns[6:10], function(x) x[itcols])),
                          st2mean = do.call("c", postmns2[1:5]),
                          st2sd = do.call("c", postmns2[6:10]))
stdat$inint <- parres$inint[as.numeric(sapply(itnames, function(x) grep(x, parres$parm)))]

ggplot(stdat, aes(x = st1mean, y = st2mean, colour = factor(inint))) + geom_point() +
  geom_errorbarh(aes(xmin = st1mean - st1sd, xmax = st1mean + st1sd), alpha = .25) +
  geom_errorbar(aes(ymin = st2mean - st2sd, ymax = st2mean + st2sd), alpha = .25) +
  xlab("Study 1 Estimate") + ylab("Study 2 Estimate") +
  geom_abline(slope = 1, intercept = 0) + facet_wrap(~ parm, scales = "free") +
  scale_color_manual(values = c("1" = "black", "0" = "red")) +
  theme(legend.position = "none")


##############
## Figure 7
## overlay mean study 2 information on posterior dist from study 1
reps1 <- summaries$reps
reps1 <- subset(reps1, iter %in% itersamp & item %in% itcols)
reps1$item <- sclabs[reps1$item]
summaries2$longthet$iter <- 1
summaries2$longthet$item <- sclabs[itcols][summaries2$longthet$item]
ggplot(reps1, aes(x = theta, y = info, group = iter)) + geom_line(alpha = 0.4, colour = '#b3cde0') + geom_line(data = summaries2$longthet, aes(x = theta, y = info), linewidth = 1.1) + facet_wrap( ~ item) + xlab("Test proficiency (z-score)") + ylab("Scale information") + theme(strip.text = element_text(size = 7))


##############
## Figure 8
## study 2 person scores using study 1 item estimates, vs study 2 model
modest <- with(summaries2, apply(tau, 2, mean))

plot(modest, scores21, xlab = "Score using Study 2 Model", ylab = "Score using Study 1 Estimates", pch = 20); abline(0, 1)
#cor(modest, scores21)


##############
## Figure 9
ss2cor <- cor(modest, smad2, use = 'pair')
plot(modest, smad2, pch = 20, xlab = "Cognitive Test Score", ylab = "Forecasting Score")



##############
## Figure 10
compdat <- which(apply(data == -999, 1, sum) == 0)
sscore <- fcast_scores$mnscore[compdat]

timepts <- seq(5, 85, 2.5)

corres <- corres_time <- rep(NA, length(timepts))

for (i in 1:length(timepts)) {
  scnow <- sapply(adapt_test, function(x) {
    runidx <- tail(which(x$runtime <= timepts[i]), 1)
    x$score[runidx]} )

  corres[i] <- cor(scnow, sscore, use = 'pair')

  scnow <- sapply(adapt_test_time, function(x) {
    runidx <- tail(which(x$runtime <= timepts[i]), 1)
    x$score[runidx]} )

  corres_time[i] <- cor(scnow, sscore, use = 'pair')  
}

cordf <- data.frame(time = rep(timepts, 2),
                    "Test.type" = rep(c("Info", "Info per min"), each = length(timepts)),
                    cor = abs(c(corres, corres_time)))

ggplot(cordf, aes(x = time, y = cor, group = Test.type, colour = Test.type)) + geom_point() + geom_line() + xlab("Minutes Elapsed") + ylab("Correlation with Forecasting Accuracy") + ylim(c(.5,.7)) + theme_bw()


##############
## Figure 11
thets <- seq(-3, 3, .1)
recs <- c(-2, 0, 2)
lutab <- matrix(summaries2$longthet$info, nrow = 20, byrow = TRUE)
lutab <- lutab[-which(colnames(data2) %in% c("percent_correct_IQ", "calibration_GK")), ]
lababb <- colnames(data2)[-which(colnames(data2) %in% c("percent_correct_IQ", "calibration_GK"))]
lababb[which(lababb == "cognitive_reflecton")] <- "cognitive_reflection"
newtimes <- median_times[!(names(median_times) %in% c("percent_correct_IQ", "calibration_GK"))]

res <- vector("list", 3)
for (i in 1:length(recs)) {
  lucol <- which(seq(-3, 3, .1) == recs[i])
  out <- cbind.data.frame(test = lababb, info = lutab[,lucol], time = newtimes)
  out$infopermin <- with(out, info / time)
  out <- out[order(out$infopermin, decreasing = TRUE),]
  out$cumtime <- cumsum(out$time)
  out$theta <- recs[i]
  res[[i]] <- out
}

res2 <- do.call("rbind", res)


ggplot(res2, aes(x = theta, y = cumtime, label = test)) + geom_text(size = 3, aes(x = theta)) + xlab("Test proficiency (sd)") + ylab("Cumulative length of testing (min)") + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) + scale_x_continuous(limits = c(-2.5, 2.5), breaks = c(-2, 0, 2)) + scale_y_continuous(limits = c(0, 100), breaks = c(0, 20, 40, 60, 80, 100))


##############
## Figure 12
ggplot(aggres$impdf, aes(x = step, y = improv)) + stat_summary(fun.data = mean_se, geom = "errorbar", width = .2) + stat_summary(fun = "mean", geom = "point") + xlab("Step of adaptive test") + ylab("Average S-score") + theme_bw()


##############
## Figure 13
aggres$aggcomp$title <- fcast_its$Title[match(aggres$aggcomp$item, fcast_its$ID)]
ggplot(aggres$aggcomp, aes(x = judgment, y = factor(agg, levels = c("Step 0", "Step 1", "Step 18"), labels = c("0", "1", "18")))) + geom_line(colour = "grey") + geom_point(colour = "grey70") + facet_wrap(~ title, scales = "free_x") + geom_point(aes(y  = factor(agg, levels = c("Step 0", "Step 1", "Step 18"), labels = c("0", "1", "18")), x = truth), color = "red", alpha = .2) + xlab("Aggregated forecast (scaled by outcome's order of magnitude)") + ylab("Step of adaptive test") + theme(
    panel.background = element_rect(fill = "transparent", colour = NA), # Transparent panel background
    plot.background = element_rect(fill = "transparent", colour = NA),  # Transparent plot background
    panel.grid.major = element_blank(),                               # Remove major gridlines
    panel.grid.minor = element_blank(),                               # Remove minor gridlines
    strip.text = element_text(size = 7)
    )


##############
## Figure A1
cordat <- data
cordat[cordat == -999] <- NA
colnames(cordat) <- sclabs
mycors <- cor(cordat, use = 'pairwise.complete')
ggcor <- cbind.data.frame(item1 = rep(sclabs, each = 20), item2 = rep(sclabs, 20), cor = round(as.numeric(mycors),2))

ggplot(ggcor[ggcor$cor < 1,], aes(x = item1, y = item2, fill = cor)) + geom_tile() + geom_text(aes(item2, item1, label=cor), size=2.5) + scale_fill_gradient(name=expression("Pearson r"), low = "#fdf6e3", high = "steelblue",
    breaks=seq(-.3, 1, by = 0.2), limits = c(-.3, 1)) + xlab("") + ylab("") +
    theme(axis.ticks=element_blank(), axis.text.x=element_text(size=7, angle=45, vjust=1, hjust = 1), axis.text.y=element_text(size=7), legend.position = "none")


##############
## Figure A2
testseq <- sapply(adapt_test, function(x) x$seen_tests)
testdf <- data.frame(sequ = rep(1:nrow(testseq), ncol(testseq)),
                     id = rep(1:ncol(testseq), each = nrow(testseq)),
                     test = as.numeric(testseq))
lababb <- sclabs[itcols]
lababb <- lababb[-which(lababb %in% c("calibration_GK", "percent_correct_IQ"))]
names(lababb) <- 1:nrow(testseq)


testseq2 <- sapply(adapt_test_time, function(x) x$seen_tests)
testdf2 <- data.frame(sequ = rep(1:nrow(testseq2), ncol(testseq2)),
                      id = rep(1:ncol(testseq2), each = nrow(testseq2)),
                      test = as.numeric(testseq2))

testdf <- rbind(testdf, testdf2)
testdf$type <- rep(c("Information", "Information per minute"), each = nrow(testdf2))

testord <- with(testdf, tapply(sequ, test, mean))

ggplot(testdf, aes(x = sequ, y = factor(test, levels = names(testord)[order(testord)]), group = id)) + geom_jitter(width = .15, height = .15, alpha = .025) + geom_line(alpha = .025) + xlab("Test sequence") + ylab("Test administered") + scale_y_discrete(labels = lababb) + facet_wrap(~ type) + theme_bw() + theme(legend.position = "none") 


##############
## Figure A3
compdat <- which(apply(data2 == -999, 1, sum) == 0)
sscore <- smad2[compdat]

timepts <- seq(5, 85, 2.5)

corres <- corres_time <- rep(NA, length(timepts))

for (i in 1:length(timepts)) {
  scnow <- sapply(adapt_test_time_rev, function(x) {
    runidx <- tail(which(x$runtime <= timepts[i]), 1)
    x$score[runidx]} )

  corres[i] <- cor(scnow, sscore, use = 'pair')
}

cordf <- data.frame(time = timepts, cor = abs(corres))

ggplot(cordf, aes(x = time, y = cor)) + geom_line() + geom_point() + xlab("Minutes Elapsed") + ylab("Correlation with Forecasting Accuracy") + ylim(c(.5,.7)) + theme_bw()


##############
## Figure A4
testseq <- sapply(adapt_test_time_rev, function(x) x$seen_tests)
testdf <- data.frame(sequ = rep(1:nrow(testseq), ncol(testseq)),
                     id = rep(1:ncol(testseq), each = nrow(testseq)),
                     test = as.numeric(testseq))
lababb <- sclabs[itcols]
lababb <- lababb[-which(lababb %in% c("calibration_GK", "percent_correct_IQ"))]
names(lababb) <- 1:nrow(testseq)

testord <- with(testdf, tapply(sequ, test, mean))

ggplot(testdf, aes(x = sequ, y = factor(test, levels = names(testord)[order(testord)]), group = id)) + geom_jitter(width = .15, height = .15, alpha = .05) + geom_line(alpha = .05) + xlab("Test sequence") + ylab("Test administered") + scale_y_discrete(labels = lababb) + theme_bw() + theme(legend.position = "none") 

