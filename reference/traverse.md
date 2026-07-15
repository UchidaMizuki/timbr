# Apply a function hierarchically to a forest

Apply a function hierarchically to a forest in the climbing or
descending direction.

## Usage

``` r
traverse(.x, .f, ..., .climb = FALSE)
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
