test_that("children", {
  library(dplyr)

  fr1 <- vec_expand_grid(key1 = rev(letters),
                         key2 = letters) %>%
    mutate(value = row_number()) %>%
    forest_by(key1, key2)
  fr2 <- vec_expand_grid(key1 = letters,
                         key3 = rev(letters)) %>%
    mutate(value = row_number()) %>%
    forest_by(key1, key3)
  fr <- rbind(fr1, fr2)

  expect_equal(fr,
               fr %>%
                 summarise() %>%
                 children())

  fr1 <- vec_expand_grid(key1 = letters,
                         key2 = rev(letters)) %>%
    mutate(value = row_number()) %>%
    forest_by(key1, key2) %>%
    summarise(value = sum(value))
  fr2 <- vec_expand_grid(key3 = rev(letters),
                         key4 = letters) %>%
    mutate(value = row_number()) %>%
    forest_by(key3, key4) %>%
    summarise(value = sum(value))
  fr <- rbind(fr1, fr2)

  expect_error(fr %>%
                 children())

  fr2_2 <- fr %>%
    children("key3") %>%
    summarise(value = sum(value))

  expect_equal(fr2, fr2_2)
})
