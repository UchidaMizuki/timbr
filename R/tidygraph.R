#' @importFrom tidygraph as_tbl_graph
#' @export
as_tbl_graph.forest <- function(x,
                                node_key = "name", ...) {
  nodes <- x$nodes

  # edges
  node_parents <- nodes$.$parent
  locs <- !vec_detect_missing(node_parents)
  edges <- tibble::tibble(from = vec_slice(node_parents, locs),
                          to = vec_slice(vec_seq_along(nodes), locs))

  # nodes
  nodes$. <- timbr_node(nodes$.$name, nodes$.$value)
  names(nodes)[names(nodes) == "."] <- node_key

  tidygraph::tbl_graph(nodes = nodes,
                       edges = edges, ...)
}
