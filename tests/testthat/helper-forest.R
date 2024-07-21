expect_equal_forest <- function(object, expected) {
  expect_equal(object$roots, expected$roots)
  expect_equal(get_nodes(object), get_nodes(expected))
  expect_equal(get_edges(object), get_edges(expected))
}
