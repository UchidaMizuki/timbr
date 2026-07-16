test_that("print", {
  library(dplyr)

  fr1 <- vec_expand_grid(key1 = letters[1:2], key2 = letters[1:2]) |>
    mutate(value = row_number()) |>
    forest_by(key1, key2) |>
    summarise(value = sum(value))

  expect_snapshot(fr1)

  fr2 <- vec_expand_grid(key3 = letters[1:2], key2 = rev(letters[1:2])) |>
    mutate(value = row_number()) |>
    forest_by(key3, key2) |>
    summarise(value = sum(value))
  fr <- rbind(fr1, fr2)

  expect_snapshot(fr)

  fr_ungrouped <- vec_expand_grid(key1 = letters[1:3]) |>
    mutate(value = row_number()) |>
    forest_by(key1)

  expect_snapshot(fr_ungrouped)
})

test_that("print with groups and sibling branches", {
  library(dplyr)

  fr1 <- vec_expand_grid(
    key1 = letters[1:2],
    key2_1 = letters[1:2],
    key3_1 = letters[1:2]
  ) |>
    mutate(value = row_number()) |>
    forest_by(key1, key2_1, key3_1) |>
    summarise(value = sum(value))

  expect_snapshot(fr1)

  fr2 <- vec_expand_grid(
    key1 = letters[1:2],
    key2_2 = letters[1:2],
    key3_2 = letters[1:2]
  ) |>
    mutate(value = row_number()) |>
    forest_by(key1, key2_2, key3_2) |>
    summarise(value = sum(value))
  fr_sum <- rbind(fr1, fr2) |>
    summarise(value = sum(value))

  expect_snapshot(fr_sum)
})
