
<!-- README.md is generated from README.Rmd. Please edit that file -->

# toRi

<!-- badges: start -->

<!-- badges: end -->

The goal of toRi is to provided a simple way to calculate temporal
orientation indices in R from data obtained from Google Trends or other
online plaforms.

## Installation

You can install the development version of toRi from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("racorreia/toRi")
```

## Example

This is a basic example of the usage framework for the toRi package:

``` r
library(toRi)
## load exmaple dataset
data(sample_data)

## check data fit for calculating temporal orientation indices
checkData(sample_data)
#> [1] "Check complete, no warnings"

## calculate temporal orientation indices from sample data
test_toi <- calculateTOI(sample_data)

## extract additional data from Google trends based on year or concept searches
#year_sv_2020 <- getYearSearches(focus.year = 2020, search.horizon = 1, api.key = "YOURAPIKEY")
#concept_sv_2020 <- getYearSearches(focus.year = 2020, api.key = "YOURAPIKEY")
```

Examples of the use of each function available in this package are
provided can be consulted by calling the help function as shown:

``` r
# Get function help
?getYearSearches
?getConceptSearches
?checkData
?calculateTOI
```
