
<!-- README.md is generated from README.Rmd. Please edit that file -->

# paletteblend <a href="https://davidhodge931.github.io/paletteblend/"><img src="man/figures/logo.png" align="right" height="139" alt="paletteblend website" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/paletteblend)](https://CRAN.R-project.org/package=paletteblend)
<!-- badges: end -->

The objective of paletteblend is to blend colours, palettes or palette
functions using blend modes, such as multiply and screen.

## Installation

Install from CRAN, or development version from
[GitHub](https://github.com/).

``` r
install.packages("ggwidth") 
pak::pak("davidhodge931/ggwidth")
```

``` r
library(paletteblend)
library(jumble)
scales::show_col(c(teal, orange, multiply(teal, orange)), ncol = 3)
```

<img src="man/figures/README-setup-1.png" alt="" width="100%" />

<img src="man/figures/README-unnamed-chunk-3-1.png" alt="" width="100%" />
<img src="man/figures/README-unnamed-chunk-4-1.png" alt="" width="100%" />
