timbr_node <- function(name, value) {
  new_rcrd(vec_recycle_common(name = name,
                              value = value),
           class = "timbr_node")
}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.timbr_node <- function(x, ...) {
  formatted <- paste0(pillar::align(pillar::style_subtle(paste0("<", field(x, "name"), "> "))),
                      field(x, "value"))
  pillar::new_pillar_shaft_simple(formatted,
                                  align = "left")
}

#' @export
format.timbr_node <- function(x, ...) {
  paste0("<", field(x, "name"), "> ", field(x, "value"))
}

#' @export
vec_ptype_abbr.timbr_node <- function(x, ...) {
  "node"
}
