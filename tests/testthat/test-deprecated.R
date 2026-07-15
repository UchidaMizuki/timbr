test_that("map_forest", {
  library(dplyr)

  fr <- vec_expand_grid(key1 = letters[1:2], key2 = letters[1:2]) |>
    mutate(value = row_number()) |>
    forest_by(key1, key2)

  f <- function(x, y) {
    x$value <- sum(y$value)
    x
  }

  lifecycle::expect_deprecated(
    fr_map_forest <- fr |>
      summarise() |>
      map_forest(f)
  )
  fr_traverse <- fr |>
    summarise() |>
    traverse(f)

  expect_equal_forest(fr_map_forest, fr_traverse)
})
