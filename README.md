## Code for *Identifying good forecasters via adaptive cognitive tests*
This repo contains the code and analyses used in the paper. The data are contained in a [separate repo for the main project.](https://github.com/forecastingresearch/fpt) 

To obtain the data and run the paper scripts, open `replication_code.R` and follow the instructions. An overview appears below.

1. Clone this repo and the data repo inside the same top-level folder. On many systems, you could open the terminal, go to the folder that should contain the repos, then issue the following commands:

```
git clone git@github.com:forecastingresearch/fpt.git
git clone git@github.com:ecmerkle/cog_adapt.git
```

This step will vary depending on how git is set up on your computer.


2. In the resulting cog_adapt folder, open R and run `source("pkgcheck.R")` to ensure that the necessary R packages are installed.

3. While still in R, run `source("compile.R")`. This step will take a long time (say, 2 hours on a cpu) because it is running multiple Bayesian models. To run individual models or parts of the analysis, see the individual targets inside `_targets.R`.

4. Once Step 3 completes, figures and other results from the paper can be obtained from the code in `replication_code.R`.

