## Code for *Identifying good forecasters via adaptive cognitive tests*
This repo contains the code and analyses used in the paper. The data are contained in a [separate repo for the main project.](https://github.com/forecastingresearch/fpt) To obtain the data and run the paper scripts, follow the instructions below.

1. Clone this repo and the data repo inside the same top-level folder. On many systems, you could open the terminal, go to the folder that should contain the repos, then issue the following commands:

```
git clone git@github.com:forecastingresearch/fpt.git
git clone git@github.com:ecmerkle/cog_adapt.git
```

This step will vary depending on how git is set up on your computer.


2. In the resulting cog_adapt folder, open R and run `source("pkgcheck.R")` to ensure that the necessary R packages are installed.

3. While still in R, run `source("compile.R")`

Step 3 will take a long time because it is running multiple Bayesian models. To run individual models or parts of the analysis, see the individual targets inside `_targets.R`. And code for figures and summaries are included in the file `summaries_figures.R`.

