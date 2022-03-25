timbr_node <- function(name, value) {
  new_rcrd(vec_recycle_common(name = name,
                              value = value),
           class = "timbr_node")
}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.timbr_node <- function(x, ...) {
  name <- field(x, "name")
  formatted <- paste0(pillar::align(pillar::style_subtle(paste0("[", name, "] "))),
                      format(x, ...))
  pillar::new_pillar_shaft_simple(formatted,
                                  align = "left")
}

#' @export
format.timbr_node <- function(x, ...) {
  field(x, "value")
}

#' @export
vec_ptype_abbr.timbr_node <- function(x, ...) {
  "node"
}
