test_that("node_name", {
  library(dplyr)

  fr <- vec_expand_grid(key1 = letters[1:2], key2 = letters[1:2]) |>
    mutate(value = row_number()) |>
    forest_by(key1, key2) |>
    summarise(value = sum(value))

  fr_labeled <- fr |>
    mutate(label = node_name())

  expect_equal(
    get_root_nodes(fr_labeled)$label,
    rep("key1", nrow(fr$roots))
  )
})

test_that("node vctrs methods", {
  library(dplyr)

  fr_int <- vec_expand_grid(key1 = letters[1:2], key2 = 1:2) |>
    mutate(value = row_number()) |>
    forest_by(key1, key2)
  fr_dbl <- vec_expand_grid(key1 = letters[1:2], key2 = c(1.5, 2.5)) |>
    mutate(value = row_number()) |>
    forest_by(key1, key2)

  node_int <- get_nodes(fr_int)$.
  node_dbl <- get_nodes(fr_dbl)$.

  combined <- vctrs::vec_c(node_int, node_dbl)
  expect_type(get_node_value(combined), "double")
  expect_equal(
    get_node_name(combined),
    c(get_node_name(node_int), get_node_name(node_dbl))
  )

  expect_equal(vctrs::vec_ptype_full(node_int), "node")
  expect_equal(vctrs::vec_ptype_abbr(node_int), "node")

  expect_equal(
    format(node_int),
    paste0("<", get_node_name(node_int), "> ", get_node_value(node_int))
  )

  expect_snapshot(vctrs::vec_ptype2(node_int, 1:3), error = TRUE)
  expect_snapshot(vctrs::vec_cast(node_int, 1:3), error = TRUE)
})
