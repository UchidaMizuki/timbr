test_that("leaves", {
  library(dplyr)

  fr1 <- vec_expand_grid(key1 = rev(letters[1:5]),
                         key2 = letters[1:5]) %>%
    mutate(value = row_number()) %>%
    forest_by(key1, key2)

  expect_equal(leaves(fr1), fr1)

  fr2 <- vec_expand_grid(key1 = letters[1:5],
                         key2 = rev(letters[1:5]),
                         key3 = letters[1:5]) %>%
    mutate(value = row_number()) %>%
    forest_by(key1, key2, key3)

  fr2_summarised <- fr2 |>
    summarise()

  fr3 <- rbind(fr1, fr2_summarised) |>
    summarise()

  expect_true(all(vec_equal(leaves(fr3)$nodes, rbind(fr1$nodes, fr2$nodes),
                            na_equal = T)))
})
