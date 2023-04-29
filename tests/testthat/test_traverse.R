test_that("traverse", {
  library(dplyr)

  fr <- expand.grid(key3 = letters,
                    key2 = letters,
                    key1 = letters,
                    stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    mutate(value1 = row_number(),
           value2 = NA_integer_) %>%
    forest_by(key1, key2, key3)

  fr1 <- fr %>%
    summarise(value1 = sum(value1))
  fr2 <- fr %>%
    summarise() %>%
    traverse(function(x, y) {
      x$value1 <- sum(y$value1)
      x
    })
  expect_identical(fr1, fr2)

  fr1 <- fr %>%
    mutate(value1 = sum(value1)) %>%
    summarise(value1 = first(value1))
  fr2 <- fr %>%
    summarise(value1 = sum(value1)) %>%
    traverse(function(x, y) {
      x$value1 <- y$value1
      x
    },
    .climb = TRUE)
  expect_identical(fr1, fr2)

  fr1 <- fr %>%
    mutate(value1 = 1) %>%
    summarise(value1 = 2) %>%
    summarise(value1 = 3)
  fr2 <- fr %>%
    mutate(value1 = NA) %>%
    summarise() %>%
    summarise(value1 = 3) %>%
    traverse(function(x, y) {
      x$value1 <- y$value1 - 1
      x
    },
    .climb = TRUE)
  expect_identical(fr1, fr2)
})
