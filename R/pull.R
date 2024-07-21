timbr_pull <- function(data, name) {
  root_nodes <- get_root_nodes(data)
  root_node_names <- node_name(root_nodes$.)

  name <- tidyselect::vars_pull(vec_unique(root_node_names), {{ name }})
  locs <- vec_equal(root_node_names, name,
                    na_equal = TRUE)
  new_roots <- vec_slice(data$roots, locs)
  new_root_nodes <- new_roots$.

  parent_node_ids <- get_parent_node_ids(data)
  groups <- vec_group_loc(parent_node_ids)
  groups <- vec_slice(groups, !vec_detect_missing(groups$key))
  group_keys <- groups$key

  node_locs <- integer()
  repeat {
    node_locs <- vec_c(new_root_nodes, node_locs)
    root_groups <- vec_slice(groups, vec_in(group_keys, new_root_nodes))
    new_root_nodes <- list_unchop(root_groups$loc)
    if (vec_is_empty(new_root_nodes)) {
      break
    }
  }

  # new_graph
  data$graph <- data$graph |>
    tidygraph::activate("nodes") |>
    dplyr::filter(dplyr::row_number() %in% node_locs)

  # new_roots
  new_roots$. <- get_root_node_ids(data)
  data$roots <- new_roots

  data
}
