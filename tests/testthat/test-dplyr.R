test_that("dplyr", {
  library(dplyr)

  df <- vec_expand_grid(key1 = letters[1:2],
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

  loc <- fr_sum1$roots$.
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
               c(".", "value1", "value"))

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

test_that("rows_update", {
  library(dplyr)

  fr <- vec_expand_grid(key1 = letters[1:3],
                        key2 = letters[1:3]) %>%
    mutate(value = row_number()) %>%
    forest_by(key1, key2) %>%
    summarise(value = sum(value))

  df <- vec_expand_grid(key1 = c("c", "a"),
                        key2 = c("b", "a", "c")) %>%
    mutate(value = row_number())

  expect_no_error(fr %>%
                    rows_update(df,
                                by = c("key1", "key2")) %>%
                    children())
  expect_no_error(fr %>%
                    rows_patch(df,
                                by = c("key1", "key2")) %>%
                    children())
  expect_error(fr %>%
                 rows_update(df,
                             by = c("key2", "key1")) %>%
                 children())
  expect_error(fr %>%
                 rows_patch(df,
                             by = c("key2", "key1")) %>%
                 children())

  df <- vec_expand_grid(key1 = c("c", "a"),
                        key2 = c("b", "a", "x")) %>%
    mutate(value = row_number())
  expect_error(fr %>%
                 rows_update(df,
                             by = c("key1", "key2")) %>%
                 children())
  expect_error(fr %>%
                 rows_patch(df,
                             by = c("key1", "key2")) %>%
                 children())
})
