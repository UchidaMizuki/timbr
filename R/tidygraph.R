#' @importFrom tidygraph as_tbl_graph
#' @export
as_tbl_graph.forest <- function(x, ...) {
  nodes <- x$nodes

  # edges
  node_parents <- nodes$node$parent
  loc <- !is.na(node_parents)
  edges <- tibble::tibble(from = node_parents[loc],
                          to = vec_seq_along(nodes)[loc])

  # nodes
  nodes$node <- timbr_node(nodes$node$name, nodes$node$value)

  tidygraph::tbl_graph(nodes = nodes,
                       edges = edges)
}
