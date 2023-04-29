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
    name <- vec_slice(data$nodes$.$name, data$roots$.)
    name <- vec_unique(name)
    stopifnot(
      rlang::is_scalar_character(name)
    )
  } else {
    name <- rlang::as_name(name)
  }

  data <- timbr_pull(data, name)
  timbr_children(data, name)
}

timbr_children <- function(data,
                           name = NULL) {
  roots <- data$roots
  nodes <- data$nodes

  new_root_keys <- drop_node(roots)

  if (!is.null(name)) {
    new_root_keys <- cbind_check(new_root_keys,
                                 !!name := vec_slice(nodes$.$value, roots$.))
  }

  # new_nodes
  new_root_locs <- vec_in(nodes$.$parent, roots$.)
  new_root_nodes <- vec_slice(nodes, new_root_locs)

  new_root_keys <- vec_slice(new_root_keys,
                             vec_match(new_root_nodes$.$parent, roots$.))

  new_root_nodes$.$parent <- NA_integer_
  vec_slice(nodes, new_root_locs) <- new_root_nodes

  node_locs <- vec_as_location(-roots$., vec_size(nodes))
  new_nodes <- vec_slice(nodes, node_locs)
  new_node_locs <- vec_seq_along(new_nodes)
  new_nodes$.$parent <- new_nodes$.$parent + new_node_locs - node_locs

  # new_roots
  new_roots <- cbind_check(new_root_keys,
                           . = vec_slice(new_node_locs,
                                         vec_detect_missing(new_nodes$.$parent)))
  new_roots <- dplyr::grouped_df(new_roots, names(new_root_keys))

  forest(new_roots, new_nodes)
}
