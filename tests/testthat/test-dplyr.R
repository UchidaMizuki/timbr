test_that("dplyr", {
  library(dplyr)

  fr <- expand.grid(key1 = letters[1:2],
                    key2 = letters[1:2],
                    key3 = letters[1:2],
                    stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    mutate(value = row_number()) %>%
    forest_by(key1, key2, key3) %>%
    summarise(value = sum(value))
  expect_s3_class(fr, "forest")

  fr1 <- fr %>%
    rows_update(expand.grid(key1 = letters[1:3],
                            key2 = letters[1:3],
                            key3 = letters[1:3],
                            stringsAsFactors = FALSE) %>%
                  as_tibble() %>%
                  mutate(value = row_number()),
                by = c("key1", "key2", "key3"))
})
