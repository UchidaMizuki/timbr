#' Climb a forest from parents to children
#'
#' Climb a forest from parents to children with one or more node names.
#'
#' @param .data A forest.
#' @param ... A list of node names to climb the forest.
#' @param .recurse Whether to search recursively by node names or not?
#' @param .deep (Deprecated) Whether to search recursively by node names or not?
#'
#' @return A forest.
#'
#' @export
climb <- function(.data, ...,
                  .recurse = TRUE,
                  .deep) {
  if (!missing(.deep)) {
    lifecycle::deprecate_warn("0.3.0", "climb(.deep)", "climb(.recurse)")
    .recurse <- .deep
  }

  names <- rlang::enquos(...)

  data <- .data
  data$nodes <- cbind_check(. = data$nodes$.,
                            loc = vec_seq_along(data$nodes))
  data <- timbr_climb(data, names, .recurse)
  data$nodes <- cbind_check(. = data$nodes$.,
                            vec_slice(drop_node(.data$nodes), data$nodes$loc))
  data
}

timbr_climb <- function(data, names, recurse) {
  if (vec_is_empty(names)) {
    data
  } else {
    name <- rlang::as_name(names[[1L]])
    names <- names[-1L]

    if (recurse) {
      nodes <- data$nodes
      root_nodes <- vec_slice(nodes, data$roots$.)
      root_node_names <- vec_unique(root_nodes$.$name)

      frs <- vec_init_along(list(), root_node_names)

      for (i in vec_seq_along(root_node_names)) {
        root_node_name <- root_node_names[[i]]
        fr <- timbr_pull(data, root_node_name)

        if (root_node_name == name) {
          if (vec_is_empty(names)) {
            frs[[i]] <- fr
          } else {
            fr <- timbr_children(fr, name)
            frs[[i]] <- climb(fr, !!!names)
          }
        } else {
          fr <- timbr_children(fr)

          if (vec_is_empty(fr$nodes)) {
            frs[[i]] <- fr
          } else {
            frs[[i]] <- climb(fr, !!name, !!!names)
          }
        }
      }

      rlang::exec(rbind, !!!frs)
    } else {
      out <- timbr_pull(data, name)

      if (!vec_is_empty(names)) {
        out <- timbr_children(out, name)
        out <- climb(out, !!!names,
                     recurse = FALSE)
      }
      out
    }
  }
}

timbr_pull <- function(data, name) {
  roots <- data$roots
  nodes <- data$nodes

  loc <- timbr_pull_loc(roots, nodes$., name)
  new_roots <- loc$new_roots
  node_locs <- loc$node_locs

  # new_nodes
  node_locs <- vec_sort(node_locs)
  new_node_locs <- vec_seq_along(node_locs)

  new_nodes <- vec_slice(nodes, node_locs)
  new_nodes$.$parent <- new_nodes$.$parent + new_node_locs - node_locs

  # new_roots
  new_root_keys <- drop_node(new_roots)
  new_roots <- cbind_check(new_root_keys,
                           . = vec_slice(new_node_locs, vec_detect_missing(new_nodes$.$parent)))

  if (dplyr::is_grouped_df(new_root_keys)) {
    new_roots <- dplyr::new_grouped_df(new_roots, group_data(new_root_keys))
  }

  forest(new_roots, new_nodes)
}

timbr_pull_loc <- function(roots, nodes, name) {
  root_nodes <- vec_slice(nodes, roots$.)
  name <- tidyselect::vars_pull(vec_unique(root_nodes$name), name)

  locs <- vec_equal(root_nodes$name, name,
                    na_equal = TRUE)
  new_roots <- vec_slice(roots, locs)
  new_root_nodes <- new_roots$.

  grps <- vec_group_loc(nodes$parent)
  grps <- vec_slice(grps, !vec_detect_missing(grps$key))
  grp_keys <- grps$key

  node_locs <- integer()
  repeat {
    node_locs <- vec_c(new_root_nodes, node_locs)
    root_grps <- vec_slice(grps, vec_in(grp_keys, new_root_nodes))
    new_root_nodes <- vec_c(!!!root_grps$loc)
    if (vec_is_empty(new_root_nodes)) {
      break
    }
  }
  list(new_roots = new_roots,
       node_locs = node_locs)
}
