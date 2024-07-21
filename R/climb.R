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
  data$nodes <- data_frame(. = data$nodes$.,
                           loc = vec_seq_along(data$nodes))
  data <- timbr_climb(data, names, .recurse)
  data$nodes <- data_frame(. = data$nodes$.,
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
            frs[[i]] <- timbr_climb(fr, names, recurse)
          }
        } else {
          fr <- timbr_children(fr)

          if (vec_is_empty(fr$nodes)) {
            frs[[i]] <- fr
          } else {
            frs[[i]] <- timbr_climb(fr, c(name, names), recurse)
          }
        }
      }

      rlang::exec(rbind, !!!frs)
    } else {
      out <- timbr_pull(data, name)

      if (!vec_is_empty(names)) {
        out <- timbr_children(out, name)
        out <- timbr_climb(out, names, recurse)
      }
      out
    }
  }
}
