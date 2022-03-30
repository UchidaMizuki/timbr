#' @importFrom tidygraph as_tbl_graph
#' @export
as_tbl_graph.forest <- function(x,
                                node_key = "name", ...) {
  nodes <- x$nodes

  # edges
  node_parents <- nodes$.$parent
  loc <- !is.na(node_parents)
  edges <- tibble::tibble(from = node_parents[loc],
                          to = vec_seq_along(nodes)[loc])

  # nodes
  nodes$. <- timbr_node(nodes$.$name, nodes$.$value)
  names(nodes)[names(nodes) == "."] <- node_key

  tidygraph::tbl_graph(nodes = nodes,
                       edges = edges, ...)
}
