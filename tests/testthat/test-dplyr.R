test_that("dplyr", {
  library(dplyr)

  df <- tidyr::expand_grid(key1 = letters[1:2],
                           key2 = letters[1:2],
                           key3 = letters[1:2]) %>%
    mutate(value = row_number())
  fr <- df %>%
    forest_by(key1, key2, key3)
  expect_s3_class(fr, "forest")

  df_sum <- df %>%
    group_by(key1, key2) %>%
    summarise(value = sum(value),
              .groups = "drop")

  fr_sum <- fr %>%
    summarise(value = sum(value))

  expect_equal(as_tibble(fr_sum), df_sum)
  expect_equal(children(fr_sum), fr)

  fr_sum1 <- fr_sum %>%
    mutate(value1 = value + 1)

  loc <- fr_sum1$roots$node
  expect_equal(vec_slice(fr_sum1$nodes$value1, loc),
               vec_slice(fr_sum1$nodes$value + 1, loc))
  expect_true(all(is.na(vec_slice(fr_sum1$nodes$value1,
                                  vec_as_location(-loc, vec_size(fr_sum1$nodes))))))
  expect_equal(fr_sum,
               fr_sum1 %>%
                 select(!value1))

  fr_sum2 <- fr_sum1 %>%
    relocate(value1)
  expect_equal(colnames(fr_sum2$nodes),
               c("node", "value1", "value"))

  df <- fr %>%
    as_tibble() %>%
    mutate(value = value * 2)

  expect_equal(fr_sum %>%
                 rows_update(df) %>%
                 children(),
               fr %>%
                 mutate(value = value * 2))
  expect_equal(fr_sum %>%
                 rows_patch(df) %>%
                 children(),
               fr)
})
