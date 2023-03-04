
<!-- README.md is generated from README.Rmd. Please edit that file -->

# timbr

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/timbr)](https://CRAN.R-project.org/package=timbr)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/UchidaMizuki/timbr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/UchidaMizuki/timbr?branch=main)
[![R-CMD-check](https://github.com/UchidaMizuki/timbr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/UchidaMizuki/timbr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

timbr provides data frames for forest (or tree) data structures. You can
create forest data structures from data frames and process them based on
their hierarchies.

## Installation

You can install the development version of timbr from
[GitHub](https://github.com/) with:

``` r
# the released version from CRAN:
install.packages("timbr")

# the development version from GitHub:
# install.packages("devtools")
devtools::install_github("UchidaMizuki/timbr")
```

## Main Functions

The main functions provided by timbr are as follows,

- `children()`
- `climb()`
- `leaves()`
- `map_forest()`
- `rbind()`

## tidyverse methods

timbr provides some tidyverse methods as follows,

- `mutate()`
- `summarise()`
- `select()` and `relocate()`
- `rows_update()` and `rows_patch()`

## Examples

``` r
library(timbr)
library(dplyr)
```

``` r
fr <- tidyr::expand_grid(key1 = letters[1:2],
                         key2 = letters[1:2],
                         key3 = letters[1:2]) |>  
  mutate(value = row_number()) |> 
  forest_by(key1, key2, key3)

fr_sum <- fr |> 
  summarise(value = sum(value)) |> 
  summarise(value = sum(value))

fr
#> # A forest: 8 nodes and 1 feature
#> # Groups:   key1, key2 [4]
#> # Trees:
#> #   key3 [8]
#>   key1  key2  .        value
#>   <chr> <chr> <node>   <int>
#> 1 a     a     <key3> a     1
#> 2 a     a     <key3> b     2
#> 3 a     b     <key3> a     3
#> 4 a     b     <key3> b     4
#> 5 b     a     <key3> a     5
#> 6 b     a     <key3> b     6
#> 7 b     b     <key3> a     7
#> 8 b     b     <key3> b     8
fr_sum
#> # A forest: 14 nodes and 1 feature
#> # Trees:
#> #   key1 [2]
#> #   └─key2 [4]
#> #     └─key3 [8]
#>   .        value
#>   <node>   <int>
#> 1 <key1> a    10
#> 2 <key1> b    26
children(fr_sum)
#> # A forest: 12 nodes and 1 feature
#> # Groups:   key1 [2]
#> # Trees:
#> #   key2 [4]
#> #   └─key3 [8]
#>   key1  .        value
#>   <chr> <node>   <int>
#> 1 a     <key2> a     3
#> 2 a     <key2> b     7
#> 3 b     <key2> a    11
#> 4 b     <key2> b    15
fr_sum |> 
  climb(key3)
#> # A forest: 8 nodes and 1 feature
#> # Trees:
#> #   key3 [8]
#>   .        value
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

``` r
fr1 <- tidyr::expand_grid(key1 = letters[1:2], 
                          key2_1 = letters[1:2],
                          key3_1 = letters[1:2]) |> 
  mutate(value = row_number()) |> 
  forest_by(key1, key2_1, key3_1) |> 
  summarise(value = sum(value))

fr2 <- tidyr::expand_grid(key1 = letters[1:2], 
                          key2_2 = letters[1:2],
                          key3_2 = letters[1:2]) |> 
  mutate(value = row_number()) |> 
  forest_by(key1, key2_2, key3_2) |> 
  summarise(value = sum(value))

fr <- rbind(fr1, fr2)
fr_sum <- fr |> 
  summarise(value = sum(value))

fr
#> # A forest: 24 nodes and 1 feature
#> # Groups:   key1 [2]
#> # Trees:
#> #   key2_1 [4]
#> #   └─key3_1 [8]
#> #   key2_2 [4]
#> #   └─key3_2 [8]
#>   key1  .          value
#>   <chr> <node>     <int>
#> 1 a     <key2_1> a     3
#> 2 a     <key2_1> b     7
#> 3 b     <key2_1> a    11
#> 4 b     <key2_1> b    15
#> 5 a     <key2_2> a     3
#> 6 a     <key2_2> b     7
#> 7 b     <key2_2> a    11
#> 8 b     <key2_2> b    15
fr_sum
#> # A forest: 26 nodes and 1 feature
#> # Trees:
#> #   key1 [2]
#> #   ├─key2_1 [4]
#> #   │ └─key3_1 [8]
#> #   └─key2_2 [4]
#> #     └─key3_2 [8]
#>   .        value
#>   <node>   <int>
#> 1 <key1> a    20
#> 2 <key1> b    52
map_forest(fr_sum,
           function(x, children) {
             x$value <- prod(children$value)
             x
           })
#> # A forest: 26 nodes and 1 feature
#> # Trees:
#> #   key1 [2]
#> #   ├─key2_1 [4]
#> #   │ └─key3_1 [8]
#> #   └─key2_2 [4]
#> #     └─key3_2 [8]
#>   .          value
#>   <node>     <int>
#> 1 <key1> a     576
#> 2 <key1> b 2822400
```
