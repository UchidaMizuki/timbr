#' @export
rbind.timbr_forest <- function(...) {
  dots <- rlang::list2(...) |>
    purrr::compact()

  if (vec_is_empty(dots)) {
    return(NULL)
  }

  graph <- dots |>
    purrr::map(\(dot) dot$graph) |>
    purrr::reduce(tidygraph::bind_graphs)

  roots <- dots |>
    purrr::map(\(dot) dot$roots) |>
    list_unchop()
  roots$. <- get_root_node_ids(graph)

  dot <- dots[[1]]
  dot$roots <- roots
  dot$graph <- graph
  dot
}
