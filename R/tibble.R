#' @importFrom tibble as_tibble
#' @export
as_tibble.timbr_forest <- function(x, ...) {
  root_nodes <- get_root_nodes(x)

  node_name <- vec_unique(get_node_name(root_nodes$.))
  root_nodes |>
    dplyr::mutate(!!node_name := node_value(),
                  .before = ".") |>
    dplyr::select(!".")
}
