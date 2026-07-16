# Apply a function hierarchically to a forest

**\[defunct\]**

`map_forest()` has been removed. Please use
[`traverse()`](https://uchidamizuki.github.io/timbr/reference/traverse.md)
instead.

## Usage

``` r
map_forest(.x, .f, ..., .climb = FALSE)
```

## Arguments

- .x:

  A forest

- .f:

  A function, formula, or vector (not necessarily atomic).

- ...:

  Additional arguments passed on to the mapped function.

- .climb:

  Climbing or descending?

## Value

A forest.
