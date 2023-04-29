test_that("climb", {
  library(dplyr)

  fr1 <- vec_expand_grid(key1 = letters,
                         key2 = rev(letters)) %>%
    mutate(value = row_number()) %>%
    forest_by(key1, key2) %>%
    summarise(value = sum(value))
  fr2 <- vec_expand_grid(key3 = rev(letters),
                         key2 = letters) %>%
    mutate(value = row_number()) %>%
    forest_by(key3, key2) %>%
    summarise(value = sum(value))
  fr <- rbind(fr1, fr2)

  expect_equal(fr %>%
                 climb(key1),
               fr1)
  expect_equal(fr %>%
                 climb(key3),
               fr2)

  expect_error(fr %>%
                 climb(key2,
                       .deep = FALSE))
  expect_equal(fr %>%
                 climb(key2) %>%
                 as_tibble() %>%
                 vec_size(),
               vec_size(as_tibble(children(fr1))) +
                 vec_size(as_tibble(children(fr2))))
  expect_equal(fr %>%
                 climb(key1, key2) %>%
                 summarise(value = sum(value)),
               fr1)
  expect_equal(fr %>%
                 climb(key3, key2) %>%
                 summarise(value = sum(value)),
               fr2)
})
