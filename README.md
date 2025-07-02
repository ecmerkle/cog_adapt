## Code for *Identifying good forecasters via adaptive cognitive tests*
This repo contains the code and analyses used in the paper. The data are contained in a [separate repo for the main project.](https://github.com/forecastingresearch/fpt) To obtain the data and run the paper scripts, follow the instructions below.

1. Clone this repo and the data repo inside the same folder. For example,

```
git clone git@github.com:forecastingresearch/fpt.git
git clone git@github.com:ecmerkle/cog_adapt.git
```


2. In the resulting cog_adapt folder, open R and run `source("compile.R")`

Step 2 will take a long time because it is running multiple Bayesian models. To run individual models or parts of the analysis, see the individual targets inside `_targets.R`. And code for figures and summaries are included in the file `summaries_figures.R`.

