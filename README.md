
<!-- README.md is generated from README.Rmd. Please edit that file -->

# timbr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/UchidaMizuki/timbr/workflows/R-CMD-check/badge.svg)](https://github.com/UchidaMizuki/timbr/actions)
[![Codecov test
coverage](https://codecov.io/gh/UchidaMizuki/timbr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/UchidaMizuki/timbr?branch=main)
<!-- badges: end -->

timbr provides data frames for forest (or tree) data structures. You can
create forest data structures from data frames and process them based on
their hierarchies.

## Installation

You can install the development version of timbr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("UchidaMizuki/timbr")
```

## tidyverse methods

-   `mutate()`
-   `summarise()`
-   `select()` and `relocate()`
-   `rows_update()` and `rows_patch()`

## Examples

``` r
library(timbr)
library(dplyr)
```

``` r
fr <- tidyr::expand_grid(key1 = letters[1:2],
                         key2 = letters[1:2],
                         key3 = letters[1:2]) %>% 
  mutate(value = row_number()) %>% 
  forest_by(key1, key2, key3)

fr
#> # A forest: 8 nodes and 1 feature
#> # Groups:   key1, key2 [4]
#> # Roots:    key3 [8]
#>   key1  key2  node     value
#>   <chr> <chr> <node>   <int>
#> 1 a     a     <key3> a     1
#> 2 a     a     <key3> b     2
#> 3 a     b     <key3> a     3
#> 4 a     b     <key3> b     4
#> 5 b     a     <key3> a     5
#> 6 b     a     <key3> b     6
#> 7 b     b     <key3> a     7
#> 8 b     b     <key3> b     8

fr_sum <- fr %>% 
  summarise(value = sum(value)) %>% 
  summarise(value = sum(value))

fr_sum
#> # A forest: 14 nodes and 1 feature
#> # Roots:    key1 [2]
#>   node     value
#>   <node>   <int>
#> 1 <key1> a    10
#> 2 <key1> b    26

children(fr_sum)
#> # A forest: 12 nodes and 1 feature
#> # Groups:   key1 [2]
#> # Roots:    key2 [4]
#>   key1  node     value
#>   <chr> <node>   <int>
#> 1 a     <key2> a     3
#> 2 a     <key2> b     7
#> 3 b     <key2> a    11
#> 4 b     <key2> b    15

fr_sum %>% 
  climb(key3)
#> # A forest: 8 nodes and 1 feature
#> # Roots:    key3 [8]
#>   node     value
#>   <node>   <int>
#> 1 <key3> a     1
#> 2 <key3> b     2
#> 3 <key3> a     3
#> 4 <key3> b     4
#> 5 <key3> a     5
#> 6 <key3> b     6
#> 7 <key3> a     7
#> 8 <key3> b     8
```
