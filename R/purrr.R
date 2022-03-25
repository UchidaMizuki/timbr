#' @importFrom purrr modify
#' @export
modify.forest <- function(.x, .f, ..., .climb = FALSE) {
  .f <- purrr::as_mapper(.f, ...)

  nodes <- .x$nodes
  node_names <- nodes$node$name
  node_parents <- nodes$node$parent
  node_data <- drop_node(nodes)

  grps <- vec_group_loc(node_parents)
  grps <- vec_slice(grps, !is.na(grps$key))

  sizes_rle <- rle(vec_slice(node_names, grps$key))$lengths
  inits_rle <- cumsum(sizes_rle) - sizes_rle

  loc <- vec_seq_along(sizes_rle)

  if (.climb) {
    loc <- rev(loc)
  }

  for (i in loc) {
    size_rle <- sizes_rle[[i]]
    rle_locs <- seq_len(size_rle)

    grp <- vec_slice(grps, inits_rle[[i]] + rle_locs)
    grp_parent <- grp$key
    grp_children <- grp$loc

    parents <- vec_slice(node_data, grp_parent)
    parents <- vec_chop(parents)

    children <- vec_chop(node_data, grp_children)

    new_node_data <- vec_init(list_of(.ptype = node_data), size_rle)

    for (j in rle_locs) {
      if (.climb) {
        j <- size_rle - j + 1L
        new_node_data[[j]] <- .f(children[[j]], parents[[j]])
      } else {
        new_node_data[[j]] <- .f(parents[[j]], children[[j]])
      }
    }
    new_node_data <- rbind_check(!!!new_node_data)

    if (.climb) {
      vec_slice(node_data, vec_c(!!!grp_children)) <- new_node_data
    } else {
      vec_slice(node_data, grp_parent) <- new_node_data
    }
  }

  .x$nodes[-1L] <- node_data
  .x
}
