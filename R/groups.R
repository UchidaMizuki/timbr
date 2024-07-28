#' @importFrom dplyr group_data
#' @export
group_data.timbr_forest <- function(.data, ...) {
  modify_roots(group_data)(.data)
}

#' @importFrom dplyr group_keys
#' @export
group_keys.timbr_forest <- function(.tbl, ...) {
  modify_roots(group_keys)(.tbl, ...)
}

#' @importFrom dplyr group_indices
#' @export
group_indices.timbr_forest <- function(.data, ...) {
  modify_roots(group_indices)(.data, ...)
}

#' @importFrom dplyr group_vars
#' @export
group_vars.timbr_forest <- function(x, ...) {
  modify_roots(group_vars)(x)
}

#' @importFrom dplyr groups
#' @export
groups.timbr_forest <- function(x, ...) {
  modify_roots(groups)(x)
}

#' @importFrom dplyr group_size
#' @export
group_size.timbr_forest <- function(x, ...) {
  modify_roots(group_size)(x)
}

#' @importFrom dplyr n_groups
#' @export
n_groups.timbr_forest <- function(x, ...) {
  modify_roots(n_groups)(x)
}

modify_roots <- function(f) {
  function(x, ...) {
    f(x$roots, ...)
  }
}
