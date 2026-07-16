test_that("map_forest is defunct", {
  expect_error(
    map_forest(1, identity),
    class = "lifecycle_error_deprecated"
  )
})
