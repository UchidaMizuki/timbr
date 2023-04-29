#' @importFrom tidygraph as_tbl_graph
#' @export
as_tbl_graph.forest <- function(x, ...) {
  nodes <- x$nodes

  # edges
  node_parents <- nodes$.$parent
  locs <- !vec_detect_missing(node_parents)
  edges <- tibble::tibble(from = vec_slice(node_parents, locs),
                          to = vec_slice(vec_seq_along(nodes), locs))

  # nodes
  nodes[[".node_name"]] <- nodes$.$name
  nodes[[".node_value"]] <- nodes$.$value
  nodes <- nodes[c(".node_name", ".node_value",
                   setdiff(names(nodes), c(".", ".node_name", ".node_value")))]

  tidygraph::tbl_graph(nodes = nodes,
                       edges = edges, ...)
}
