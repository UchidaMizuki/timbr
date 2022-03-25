test_that("as_forest", {
  df <- expand.grid(key3 = letters, key2 = letters, key1 = letters,
                    stringsAsFactors = FALSE)
  df$value1 <- vec_seq_along(df)
  df$value2 <- vec_seq_along(df) + 1
  df <- dplyr::rowwise(df, key1, key2, key3)

  fr <- as_forest(df)
  expect_s3_class(fr, "forest")
})

test_that("rbind", {
  library(dplyr)

  fr1 <- expand.grid(key3 = letters[1:2],
                     key2 = letters[1:3],
                     key1 = letters[1:4]) %>%
    mutate(value = row_number()) %>%
    rowwise(key1, key2, key3) %>%
    as_forest() %>%
    summarise(values = list(value))
  fr2 <- expand.grid(key2 = letters[1:5],
                     key1 = letters[1:6]) %>%
    mutate(value = row_number()) %>%
    rowwise(key1, key2) %>%
    as_forest()

  expect_equal(rbind(fr1, rbind(fr1, fr2)),
               rbind(rbind(fr1, fr1), fr2))
})
