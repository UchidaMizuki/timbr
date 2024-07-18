#' Leaf nodes of a forest
#'
#' @param data A forest.
#'
#' @return A forest.
#'
#' @export
leaves <- function(data) {
  roots <- data$roots
  nodes <- data$nodes

  node_parents <- nodes$.$parent
  node_parents <- vec_slice(node_parents, !vec_detect_missing(node_parents))

  if (vec_is_empty(node_parents)) {
    data
  } else {
    node_locs <- vec_as_location(-node_parents, vec_size(nodes))

    data_root <- data
    data_root$nodes <- data_root$nodes["."]
    root_locs <- roots$.
    data_root$nodes$root <- vec_init_along(NA_integer_, data_root$nodes)
    vec_slice(data_root$nodes$root, root_locs) <- root_locs

    data_root <- traverse(data_root,
                          function(x, y) {
                            x$root <- y$root
                            x
                          },
                          .climb = TRUE)
    needles <- vec_slice(data_root$nodes$root, node_locs)
    new_roots <- vec_slice(roots,
                           vec_match(needles, roots$.))
    new_roots$. <- vec_seq_along(new_roots)

    new_nodes <- vec_slice(nodes, node_locs)
    new_nodes$.$parent <- vec_init_along(NA_integer_, new_nodes)

    new_forest(new_roots, new_nodes)
  }
}
