#' Children of the forest
#'
#' Convert a forest into a forest consisting of its child nodes.
#'
#' @param data A forest.
#' @param name `NULL` (default) or a scalar character specifying the node name
#' of child nodes.
#'
#' @return A forest.
#'
#' @export
children <- function(data,
                     name = NULL) {
  name <- rlang::enquo(name)

  if (rlang::quo_is_null(name)) {
    root_nodes <- get_root_nodes(data)
    name <- vec_unique(node_name(root_nodes$.))
  }

  data <- timbr_pull(data, {{ name }})

  roots <- get_root_nodes(data)[names(data$roots)]
  name <- vec_unique(node_name(roots$.))
  roots <- data_frame(drop_node(roots),
                      !!name := node_value(roots$.))

  root_node_ids <- get_root_node_ids(data)
  parent_node_ids <- get_parent_node_ids(data) |>
    purrr::keep(\(x) x %in% root_node_ids)

  data$roots <- vec_slice(roots,
                          vec_match(parent_node_ids, root_node_ids)) |>
    dplyr::grouped_df(names(drop_node(roots)))
  data$graph <- data$graph |>
    tidygraph::activate("nodes") |>
    dplyr::filter(!tidygraph::node_is_root())
  data$roots$. <- get_root_node_ids(data)
  data
}
