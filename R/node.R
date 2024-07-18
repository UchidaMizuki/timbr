node <- function(name, value) {
  new_rcrd(df_list(name = name,
                   value = value),
           class = "timbr_node")
}

#' Get node names
#'
#' @param x Nodes.
#'
#' @return A character vector.
#'
#' @export
node_name <- function(x) {
  field(x, "name")
}

#' Get node values
#'
#' @param x Nodes.
#'
#' @return A vector.
#'
#' @export
node_value <- function(x) {
  field(x, "value")
}

#' @export
vec_ptype2.timbr_node <- function(x, y, ..., x_arg = "", y_arg = "") {
  UseMethod("vec_ptype2.timbr_node")
}

#' @export
vec_ptype2.timbr_node.timbr_node <- function(x, y, ..., x_arg = "", y_arg = "") {
  name <- vec_ptype2(node_name(x), node_name(y))
  value <- vec_ptype2(node_value(x), node_value(y))
  node(name, value)
}

#' @export
vec_cast.timbr_node <- function(x, to, ...) {
  UseMethod("vec_cast.timbr_node")
}

#' @export
vec_cast.timbr_node.timbr_node <- function(x, to, ...) {
  name <- vec_cast(node_name(x), node_name(to))
  value <- vec_cast(node_value(x), node_value(to))
  node(name, value)
}

#' @export
pillar_shaft.timbr_node <- function(x, ...) {
  formatted <- paste0(pillar::align(pillar::style_subtle(paste0("<", node_name(x), "> "))),
                      node_value(x))
  pillar::new_pillar_shaft_simple(formatted,
                                  align = "left")
}

#' @export
format.timbr_node <- function(x, ...) {
  paste0("<", node_name(x), "> ", node_value(x))
}

#' @export
vec_ptype_full.timbr_node <- function(x, ...) {
  "node"
}

#' @export
vec_ptype_abbr.timbr_node <- function(x, ...) {
  "node"
}
