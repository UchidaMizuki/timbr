test_that("groups", {
  library(dplyr)

  fr <- vec_expand_grid(key1 = letters[1:2], key2 = letters[1:2]) |>
    mutate(value = row_number()) |>
    forest_by(key1, key2) |>
    summarise(value = sum(value))

  expect_equal(group_vars(fr), group_vars(fr$roots))
  expect_equal(group_data(fr), group_data(fr$roots))
  expect_equal(group_keys(fr), group_keys(fr$roots))
  expect_equal(group_indices(fr), group_indices(fr$roots))
  expect_equal(groups(fr), groups(fr$roots))
  expect_equal(group_size(fr), group_size(fr$roots))
  expect_equal(n_groups(fr), n_groups(fr$roots))
})
