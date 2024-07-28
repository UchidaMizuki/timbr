test_that("as_forest", {
  library(dplyr)

  df <- vec_expand_grid(key1 = letters[1:2],
                        key2 = letters[1:2]) %>%
    mutate(value = 1) %>%
    rowwise(key1, key2)

  expect_s3_class(as_forest(df), "timbr_forest")
})

test_that("rbind", {
  library(dplyr)

  fr1 <- vec_expand_grid(key1 = letters[1:2],
                         key2 = letters[1:3]) %>%
    mutate(value = row_number()) %>%
    forest_by(key1, key2) %>%
    summarise(value = sum(value))

  fr2 <- vec_expand_grid(key3 = letters[1:2],
                         key4 = letters[1:2]) %>%
    mutate(value = row_number()) %>%
    forest_by(key3, key4) %>%
    summarise(value = sum(value))

  expect_equal_forest(rbind(fr1, rbind(fr1, fr2)),
                      rbind(rbind(fr1, fr1), fr2))
})

test_that("children-climb", {
  library(dplyr)

  fr1 <- vec_expand_grid(key1 = letters[1:2],
                         key2 = letters[1:3],
                         key3 = letters[1:4]) %>%
    mutate(value = row_number()) %>%
    forest_by(key1, key2, key3)

  fr2 <- vec_expand_grid(key1 = letters[1:5],
                         key2 = letters[1:6]) %>%
    mutate(value = row_number()) %>%
    forest_by(key1, key2)


  fr1_sum <- fr1 %>%
    summarise(value = sum(value)) %>%
    summarise(value = sum(value))

  fr2_sum <- fr2 %>%
    summarise(value = sum(value))

  fr <- rbind(fr1 %>%
                summarise(value = sum(value)),
              fr2)
  fr_sum <- rbind(fr1_sum, fr2_sum)

  expect_equal_forest(fr_sum %>%
                        children(),
                      fr)
  expect_equal_forest(fr_sum %>%
                        climb(key1, key2, key3),
                      fr1)
})
