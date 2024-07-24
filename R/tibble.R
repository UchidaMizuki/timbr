#' @importFrom tibble as_tibble
#' @export
as_tibble.timbr_forest <- function(x, ...) {
  get_root_nodes(x)
}
