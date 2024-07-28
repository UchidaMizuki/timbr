#' Leaf nodes of a forest
#'
#' @param data A forest.
#'
#' @return A forest.
#'
#' @export
leaves <- function(data) {
  root_node_ids <- get_root_node_ids(data)

  data$graph <- data$graph |>
    tidygraph::activate("nodes") |>
    dplyr::mutate(.rows = dplyr::row_number())
  data <- data |>
    traverse(
      function(x, y) {
        x$.rows <- y$.rows
        x
      },
      .climb = TRUE
    )
  data$graph <- data$graph |>
    tidygraph::activate("nodes") |>
    dplyr::filter(tidygraph::node_is_leaf())

  data$roots <- vec_slice(data$roots,
                          vec_match(get_nodes(data)$.rows, root_node_ids))
  data$roots$. <- get_root_node_ids(data)
  data |>
    dplyr::select(!".rows")
}
