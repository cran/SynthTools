# SynthTools

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/SynthTools)](https://cran.r-project.org/package=SynthTools)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of SynthTools is to make measuring the utility of partially synthetic and multiple imputed data sets easier.  SynthTools includes functions that check to make sure original and derived data sets have comparable attributes, compute overall and variable-specific perturbation rates, and compute standard errors and confidence intervals for continuous and categorical variables.
## Installation

You can install the released version of SynthTools from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("SynthTools")
```

## Example

This is a basic example which shows you how to check the comparability of an observed data set and a data set derived from it.  PPA is the observed data set and PPAps1 is the partially synthetic data set derived from PPA:

``` r
library(SynthTools)
dataComp(PPA, PPAps1)
```

