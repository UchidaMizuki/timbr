node <- function(name, value) {
  new_rcrd(df_list(name = name,
                   value = value),
           class = "timbr_node")
}

get_node_name <- function(x) {
  field(x, "name")
}

get_node_value <- function(x) {
  field(x, "value")
}

#' Get node names
#'
#' @return A character vector.
#'
#' @export
node_name <- function() {
  get_node_name(dplyr::pick(".")$.)
}

#' Get node values
#'
#' @return A vector.
#'
#' @export
node_value <- function() {
  get_node_value(dplyr::pick(".")$.)
}

#' @export
vec_ptype2.timbr_node <- function(x, y, ..., x_arg = "", y_arg = "") {
  UseMethod("vec_ptype2.timbr_node")
}

#' @export
vec_ptype2.timbr_node.timbr_node <- function(x, y, ..., x_arg = "", y_arg = "") {
  name <- vec_ptype2(get_node_name(x), get_node_name(y))
  value <- vec_ptype2(get_node_value(x), get_node_value(y))
  node(name, value)
}

#' @export
vec_cast.timbr_node <- function(x, to, ...) {
  UseMethod("vec_cast.timbr_node")
}

#' @export
vec_cast.timbr_node.timbr_node <- function(x, to, ...) {
  name <- vec_cast(get_node_name(x), get_node_name(to))
  value <- vec_cast(get_node_value(x), get_node_value(to))
  node(name, value)
}

#' @export
pillar_shaft.timbr_node <- function(x, ...) {
  formatted <- paste0(pillar::align(pillar::style_subtle(paste0("<", get_node_name(x), "> "))),
                      get_node_value(x))
  pillar::new_pillar_shaft_simple(formatted,
                                  align = "left")
}

#' @export
format.timbr_node <- function(x, ...) {
  paste0("<", get_node_name(x), "> ", get_node_value(x))
}

#' @export
vec_ptype_full.timbr_node <- function(x, ...) {
  "node"
}

#' @export
vec_ptype_abbr.timbr_node <- function(x, ...) {
  "node"
}
