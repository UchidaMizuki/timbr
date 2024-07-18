#' @importFrom tibble as_tibble
#' @export
as_tibble.timbr_forest <- function(x, ...) {
  roots <- x$roots
  nodes <- x$nodes

  # nodes
  root_nodes <- vec_slice(nodes, roots$.)
  root_node_names <- vec_unique(root_nodes$.$name)
  root_node_values <- root_nodes$.$value
  root_node_data <- drop_node(root_nodes)
  stopifnot(
    rlang::is_scalar_character(root_node_names)
  )

  out <- data_frame(roots[group_vars(x)],
                    !!root_node_names := root_node_values,
                    root_node_data)
  as_tibble(out, ...)
}
