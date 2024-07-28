#' @export
rows_update.timbr_forest <- function(x, y,
                                     by = NULL, ...) {
  by <- timbr_common_by(by, x, y)
  timbr_rows(dplyr::rows_update, x, y, by, ...)
}

#' @export
rows_patch.timbr_forest <- function(x, y,
                                    by = NULL, ...) {
  by <- timbr_common_by(by, x, y)
  timbr_rows(dplyr::rows_patch, x, y, by, ...)
}

timbr_common_by <- function(by = NULL,
                            x, y) {
  if (!is.null(by)) {
    return(by)
  }

  names_x <- c(names(drop_node(x$roots)),
               get_node_name(get_nodes(x)$.))

  by <- intersect(names(y), names_x)

  # Source: https://github.com/tidyverse/dplyr/blob/main/R/join-common-by.R
  by_quoted <- encodeString(by, quote = "\"")
  if (length(by_quoted) == 1L) {
    by_code <- by_quoted
  }
  else {
    by_code <- paste0("c(", paste(by_quoted, collapse = ", "), ")")
  }
  cli::cli_inform("Matching, by = {by_code}")

  by
}

timbr_rows <- function(f, x, y, by, ...) {
  root_nodes <- get_root_nodes(x)
  x$graph <- x$graph |>
    tidygraph::activate("nodes") |>
    dplyr::mutate(.rows = dplyr::row_number())
  new_nodes <- x |>
    climb(!!!setdiff(by, names(drop_node(root_nodes)))) |>
    tibble::as_tibble() |>
    dplyr::ungroup() |>
    f(y, by, ...) |>
    dplyr::select(!dplyr::all_of(by))

  x$graph <- x$graph |>
    tidygraph::activate("nodes") |>
    quiet_focus(dplyr::row_number() %in% new_nodes$.rows) |>
    dplyr::mutate(new_nodes) |>
    tidygraph::unfocus() |>
    dplyr::select(!".rows")
  x
}
