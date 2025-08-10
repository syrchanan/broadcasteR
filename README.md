
<!-- README.md is generated from README.Rmd. Please edit that file -->

# connoR

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

This package encapsulates many different utility functions for I use for
analyzing TV ratings and metrics, analysis, visualization, and others.

## Installation

You can install the development version of connoR like so:

``` r

devtools::install_github("syrchanan/connoR")
```

## Example

This is a basic example which shows you how to use any of the `media_`
utility function.

Call a function with the `media_` prefix, then leave the target metric
as an NA value, and fill in all the others.

Below is an example calculation of AA rating from UE and AA.

``` r

# Load the library
library(connoR)

media_aa_1(rating = NA, ue = 1000, aa = 200)
```
