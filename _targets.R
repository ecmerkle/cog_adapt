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
  )
)
