
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LCBC Conversions <img src="man/figures/hex.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/LCBC-UiO/Conversions.svg?branch=master)](https://travis-ci.org/LCBC-UiO/Conversions)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/LCBC-UiO/Conversions?branch=master&svg=true)](https://ci.appveyor.com/project/LCBC-UiO/Conversions)
[![Codecov test
coverage](https://codecov.io/gh/LCBC-UiO/Conversions/branch/master/graph/badge.svg)](https://codecov.io/gh/LCBC-UiO/Conversions?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/Conversions)](https://CRAN.R-project.org/package=Conversions)
<!-- badges: end -->

The repository contains functions to run coversions on certain raw data
in the database of LCBC.

The conversions covered so far:

  - iq-functions - [conversions from raw to T, scaled and fullscale IQ
    scores (outdated
    documentation)](https://lcbc-uio.github.io/Conversions/iq.html)  
  - bloodpress-functions - [blood pressure
    conversions](https://lcbc-uio.github.io/Conversions/bloodpress.html)  
  - bmi-functions - Body mass index calculation \[no documentation yet\]

Thee functions are not properly cleaned and optimised yet. They are
remnants of old scripts. There are no unit tests.

Vignettes are written, but are old and do not explain the functions
well, just the general consept.

## Installation

the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("LCBC-UiO/Conversions")
```

## Documentation

package documentation can be found
[here](https://lcbc-uio.github.io/Conversions/)
