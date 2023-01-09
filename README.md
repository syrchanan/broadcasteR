
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
#> Downloading GitHub repo syrchanan/chanan.utils@HEAD
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>          checking for file 'C:\Users\cdawg\AppData\Local\Temp\RtmpmUJD3W\remotes9d443be3939\syrchanan-chanan.utils-c4e344b/DESCRIPTION' ...     checking for file 'C:\Users\cdawg\AppData\Local\Temp\RtmpmUJD3W\remotes9d443be3939\syrchanan-chanan.utils-c4e344b/DESCRIPTION' ...   ✔  checking for file 'C:\Users\cdawg\AppData\Local\Temp\RtmpmUJD3W\remotes9d443be3939\syrchanan-chanan.utils-c4e344b/DESCRIPTION' (682ms)
#>       ─  preparing 'chanan.utils':
#>    checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   ✔  checking DESCRIPTION meta-information
#>       ─  checking for LF line-endings in source and make files and shell scripts
#>       ─  checking for empty or unneeded directories
#>       ─  building 'chanan.utils_0.0.1.000.tar.gz'
#>      
#> 
#> Installing package into 'C:/Users/cdawg/AppData/Local/Temp/RtmpIt9uVh/temp_libpath95205ff97dbb'
#> (as 'lib' is unspecified)
```

## Example

This is a basic example which shows you how to use any of the `media_`
utility function.

Call a function with the “media\_” prefix, then leave the target metric
as an NA value, and fill in all the others.

Below is an example calculation of AA rating from UE and AA.

``` r
# Load the library
library(chanan.utils)

media_aa_1(rating = NA, ue = 1000, aa = 200)
#> [1] 0.2
```
