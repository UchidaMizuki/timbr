#' Apply a function hierarchically to a forest
#'
#' Apply a function hierarchically to a forest in the climbing or descending direction.
#'
#' @param .x A forest
#' @param .f A function, formula, or vector (not necessarily atomic).
#' @param ... Additional arguments passed on to the mapped function.
#' @param .climb Climbing or descending?
#'
#' @return A forest.
#'
#' @export
traverse <- function(.x, .f, ...,
                     .climb = FALSE) {
  .f <- purrr::as_mapper(.f, ...)

  nodes <- get_nodes(.x)
  edges <- get_edges(.x)

  groups <- vec_group_loc(get_parent_node_ids(.x))
  groups <- vec_slice(groups,
                      vec_order(groups$key,
                                direction = if (.climb) "desc" else "asc"))

  node_names <- node_name(nodes$.)
  group_rle <- vec_group_rle(vec_slice(node_names, groups$key))
  sizes_group_rle <- field(group_rle, "length")

  loc_start_group_rle <- cumsum(sizes_group_rle) - sizes_group_rle
  loc_group_rle <- vec_seq_along(sizes_group_rle)

  for (i in loc_group_rle) {
    size_group_rle <- sizes_group_rle[[i]]
    loc_size_group_rle <- seq_len(size_group_rle)

    group <- vec_slice(groups, loc_start_group_rle[[i]] + loc_size_group_rle)
    group_parent <- group$key
    group_children <- group$loc

    parents <- vec_chop(vec_slice(nodes, group_parent))
    children <- vec_chop(nodes, group_children)

    new_nodes <- vec_init(list_of(.ptype = nodes), size_group_rle)

    for (j in loc_size_group_rle) {
      if (.climb) {
        new_nodes[[j]] <- .f(children[[j]], parents[[j]])
      } else {
        new_nodes[[j]] <- .f(parents[[j]], children[[j]])
      }
    }
    new_nodes <- list_unchop(new_nodes)

    if (.climb) {
      vec_slice(nodes, list_unchop(group_children)) <- new_nodes
    } else {
      vec_slice(nodes, group_parent) <- new_nodes
    }
  }
  .x$graph <- .x$graph |>
    tidygraph::activate("nodes") |>
    dplyr::mutate(nodes)
  .x
}
