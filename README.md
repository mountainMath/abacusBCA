
<!-- README.md is generated from README.Rmd. Please edit that file -->

# abacusBCA

<!-- badges: start -->
<!-- badges: end -->

The abacusBCA package facilitates data access and basic processing of
[BC Assessment data hosted on UBC
ABACUS](https://abacus.library.ubc.ca/dataset.xhtml?persistentId=hdl%3A11272.1%2FAB2%2FLAPUAB)
for research purposes.

## Reference

[abacusBCA home page and reference
guide](https://mountainmath.github.io/abacusBCA/index.html)

## Installation

You can install the latest version of abacusBCA from
[GitHub](https://CRAN.R-project.org) with:

``` r
remotes::install_github("mountainmath/abacusBCA")
```

## Access token and cache path

Accessing BCA data needs special privileges, the package needs your
ABACUS access token in order to download the data. You can pass the
access token in your data calls, but the preferred way is to set it in
the `ABACUS_API_TOKEN` environment variable in your `~/.Renviron`.

The package also requires the user to set a local path where to store
the BCA data. This can be passed in the data calls, but the preferred
way is to set it in your `ABACUS_CACHE_PATH` environment variable in
your `~/.Renviron`.

``` r
ABACUS_API_TOKEN='<your abacus api token>'
ABACUS_CACHE_PATH="<path on your computer where to store BCA database>"
```

## Example

For example, to access the sales data from the 2022 data dump:

``` r
library(abacusBCA)
library(dplyr)

sales_data_2021 <- list_bca_datasets() %>%
  filter(Year==2022) %>%
  get_bca_data("sales",version=.)
```
