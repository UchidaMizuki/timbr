#' @export
rbind.forest <- function(..., deparse.level = 1) {
  frs <- rlang::list2(...)
  size_frs <- vec_size(frs)

  new_roots <- vec_init(list(), size_frs)
  new_nodes <- vec_init(list(), size_frs)

  size_nodes <- 0L
  for (i in seq_len(size_frs)) {
    fr <- frs[[i]]
    roots <- fr$roots
    nodes <- fr$nodes

    roots$. <- roots$. + size_nodes
    new_roots[[i]] <- roots

    nodes$.$parent <- nodes$.$parent + size_nodes
    new_nodes[[i]] <- nodes

    size_nodes <- size_nodes + vec_size(nodes)
  }

  new_roots <- rbind_check(!!!new_roots)
  loc <- which(names(new_roots) == ".")
  new_roots <- new_roots[c(vec_as_location(-loc, ncol(new_roots)), loc)]

  new_nodes <- rbind_check(!!!new_nodes)
  stopifnot(
    vec_is_empty(intersect(names(drop_node(new_roots)),
                           names(drop_node(new_nodes))))
  )

  forest(new_roots, new_nodes)
}
