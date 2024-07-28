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
  timbr_climb(.data, names, .recurse)
}

timbr_climb <- function(data, names, recurse) {
  if (vec_is_empty(names)) {
    return(data)
  }

  name <- rlang::as_name(names[[1]])
  names <- names[-1]

  if (recurse) {
    root_nodes <- get_root_nodes(data)
    root_node_names <- vec_unique(get_node_name(root_nodes$.))

    forests <- vec_init_along(list(), root_node_names)

    for (i in vec_seq_along(root_node_names)) {
      root_node_name <- root_node_names[[i]]
      forest <- timbr_pull(data, root_node_name)

      if (root_node_name == name) {
        if (vec_is_empty(names)) {
          forests[[i]] <- forest
        } else {
          forest <- children(forest, name)
          forests[[i]] <- timbr_climb(forest, names, recurse)
        }
      } else {
        node_name <- vec_unique(get_node_name(get_root_nodes(forest)$.))

        forest <- children(forest)

        roots <- forest$roots |>
          dplyr::ungroup() |>
          dplyr::select(!dplyr::all_of(node_name))
        forest$roots <- roots |>
          grouped_df_roots()

        forests[[i]] <- timbr_climb(forest, c(name, names), recurse)
      }
    }
    rlang::exec(rbind.timbr_forest, !!!forests)
  } else {
    forest <- timbr_pull(data, name)

    if (!vec_is_empty(names)) {
      forest <- children(forest, name)
      forest <- timbr_climb(forest, names, recurse)
    }
    forest
  }
}
