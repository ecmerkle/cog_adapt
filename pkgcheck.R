pkgs <- c("ggplot2", "GGally", "bayesplot", "targets", "tarchetypes", "tidyverse", "rstan", "numDeriv")

ips <- installed.packages()

for (x in pkgs) {
  if (!(x %in% ips[, 'Package'])) {
    install.packages(x)
  }
}
