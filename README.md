
<!-- README.md is generated from README.Rmd. Please edit that file -->

# abacusBCA

<!-- badges: start -->
<!-- badges: end -->

The abacusBCA package facilitates data access and basic processing of BC
Assessment data hosted on UBC ABACUS for research purposes.

## Installation

You can install the latest version of abacusBCA from
[GitHub](https://CRAN.R-project.org) with:

``` r
remotes::install_github("mountainmath/abacusBCA")
```

## Example

For example, to access the sales data from the 2021 data dump:

``` r
library(abacusBCA)

sales_data <- get_bca_data("sales")
```
