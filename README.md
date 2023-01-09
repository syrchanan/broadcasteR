
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chanan.utils

<!-- badges: start -->
<!-- badges: end -->

The goal of chanan.utils is to encapsulate many utility functions that I
use often. There are two major sections to this package: media, and
other. Media functions all relate to and calculate Nielsen formulas,
whereas other functions are a random smattering of utilities.

## Installation

You can install the development version of chanan.utils like so:

``` r
devtools::install_github("syrchanan/chanan.utils")
```

## Example

This is a basic example which shows you how to use any of the `media_`
utility function.

Call a function with the `media_` prefix, then leave the target metric
as an NA value, and fill in all the others.

Below is an example calculation of AA rating from UE and AA.

``` r
# Load the library
library(chanan.utils)

media_aa_1(rating = NA, ue = 1000, aa = 200)
#> [1] 0.2
```
