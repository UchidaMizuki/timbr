#' @importFrom tibble as_tibble
#' @export
as_tibble.forest <- function(x, ...,
                             .rows = NULL,
                             .name_repair = c("check_unique", "unique", "universal", "minimal"),
                             rownames = pkgconfig::get_config("tibble::rownames", NULL)) {
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

  out <- cbind_check(roots[group_vars(x)],
                     !!root_node_names := root_node_values,
                     root_node_data)
  as_tibble(out, ...,
            .rows = .rows,
            .name_repair = .name_repair,
            rownames = rownames)
}
