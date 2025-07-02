library(targets)
library(tarchetypes)
source("R/functions.R")
source("R/utils.R")
source("R/bayes_update_score.R")
source("R/_density and information functions.R")
source("R/adaptive.R")

# Uncomment below to use local multicore computing
# when running tar_make_clustermq().
options(clustermq.scheduler = "multicore")

# Uncomment below to deploy targets to parallel jobs
# on a Sun Grid Engine cluster when running tar_make_clustermq().
# options(clustermq.scheduler = "sge", clustermq.template = "sge.tmpl")

# These packages only get loaded if a target needs to run.
tar_option_set(
  packages = c("rstan", "rmarkdown", "ggplot2", "GGally", "bayesplot")
)

# future::plan(future::multisession)

list(
  tar_target(
    model_file,
    # Returns the paths to the Stan source file.
    # cmdstanr skips compilation if the model is up to date.
    compile_model("stan/betairt.stan"),
    # Do not run on a parallel worker:
    deployment = "main"
  ),
  tar_target(
    model_nb,
    compile_model("stan/betairt_nobounds.stan"),
    deployment = "main"
  ),
  ## data and models for pilot study 1
  tar_target(
    median_times,
    read_times()
  ),
  tar_target(
    theta_values,
    seq(-3, 3, .1)
  ),
  tar_target(
    data,
    read_data(bounds = TRUE),
    deployment = "main"
  ),
  tar_target(
    fcast_scores,
    read_fscores(data$pilotsubs)
  ),
  tar_target(
    fit,
    fit_model(data$data, model_file, model_type = "beta")
  ),
  tar_target(
    postmns,
    summ_params(fit)
  ),
  tar_target(
    summaries,
    summ_outputs(data$data, fit, theta_values, postmns, model_type = "beta")
  ),
  ## tar_target(
  ##   data_nb,
  ##   read_data(bounds = FALSE),
  ##   deployment = "main"
  ## ),
  ## tar_target(
  ##   fit_nb,
  ##   fit_model(data_nb, model_nb, model_type = "beta_nobound")
  ## ),
  ## tar_target(
  ##   postmns_nb,
  ##   summ_params(fit_nb)
  ## ),
  ## tar_target(
  ##   summaries_nb,
  ##   summ_outputs(data_nb, fit_nb, theta_values, postmns_nb, model_type = "beta_nobound")
  ## ),
  ## data and models from main study 2
  tar_target(
    data2,
    read_data2(bounds = TRUE, lbound = data$lbound, ubound = data$ubound)
  ),
  tar_target(
    scores21,
    score_study2(data2$dat, data$data, postmns)
  ),
  tar_target(
    fit2,
    fit_model(data2$dat, model_file, model_type = "beta")
  ),
  tar_target(
    postmns2,
    summ_params(fit2)
  ),
  tar_target(
    summaries2,
    summ_outputs(data2$dat, fit2, theta_values, postmns2, model_type = "beta")
  ),
  ## adaptive test on pilot study participants with complete data,
  ## using main study item estimates
  tar_target(
    adapt_test,
    adapt12(data$data, data2, postmns2, median_times, use_times = FALSE)
  ),
  tar_target(
    adapt_test_time,
    adapt12(data$data, data2, postmns2, median_times, use_times = TRUE)
  ),
  tar_target(
    adapt_test2,
    adapt12(data2$dat, data2, postmns2, median_times, use_times = FALSE)
  ),
  tar_target(
    adapt_test_time2,
    adapt12(data2$dat, data2, postmns2, median_times, use_times = TRUE)
  ),
  tar_target(
    data3,
    read_data3(data2$dat)
  ),
  tar_target(
    fit3,
    fit_model(data3, model_file, model_type = "beta")
  ),
  tar_target(
    postmns3,
    summ_params(fit3)
  ),
  tar_target(
    summaries3,
    summ_outputs(data3, fit3, theta_values, postmns3, model_type = "beta")
  ),
  tar_target(
    mtimes3,
    read_times3(data3)
  ),
  tar_target(
    adapt_test3,
    adapt12(data3, data3, postmns3, mtimes3, use_times = FALSE)
  )
  #tar_render(report, "writeup.Rmd"),
  #tar_force(
  #  manuscript,
  #  system('knit2pdf ms.Rnw'),
  #  force = 1 > 0, priority = 0
  #)
  #tar_render(prereg, "prereg.Rmd")
  #tar_render(pres, "slides.Rmd")
)
