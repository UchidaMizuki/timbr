#' Apply a function hierarchically to a forest
#'
#' Apply a function hierarchically to a forest in the climbing or descending direction.
#'
#' @param .x A forest
#' @param .f A function, formula, or vector (not necessarily atomic).
#' @param ... Additional arguments passed on to the mapped function.
#' @param .climb Climbing or descending?
#'
#' @return A forest.
#'
#' @export
map_forest <- function(.x, .f, ...,
                       .climb = FALSE) {
  lifecycle::deprecate_warn("0.3.0", "map_forest()", "traverse()")

  traverse(.x, .f, ...,
           .climb = .climb)
}
