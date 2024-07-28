new_forest <- function(roots, graph,
                       class = character()) {
  structure(list(roots = roots,
                 graph = graph),
            class = c(setdiff(class, "timbr_forest"), "timbr_forest"))
}

#' Coerce to a forest
#'
#' @param x An object.
#' @param ... Unused, for extensibility.
#'
#' @return A forest.
#'
#' @export
as_forest <- function(x, ...) {
  UseMethod("as_forest")
}

#' @export
as_forest.rowwise_df <- function(x, ...) {
  rlang::check_dots_empty()

  group_vars <- dplyr::group_vars(x)
  x <- dplyr::ungroup(x)

  groups <- x[group_vars]
  x <- x[!names(x) %in% group_vars]

  if (vec_duplicate_any(groups)) {
    cli::cli_abort("Groups must be unique.")
  }

  size_group_vars <- vec_size(group_vars)
  roots <- data_frame(groups[-size_group_vars],
                      . = vec_seq_along(x)) |>
    dplyr::grouped_df(group_vars[-size_group_vars])

  nodes <- data_frame(. = node(name = group_vars[[size_group_vars]],
                               value = groups[[size_group_vars]]),
                      x)
  graph <- tidygraph::tbl_graph(nodes = nodes)

  new_forest(roots, graph, ...)
}

#' @export
as_forest.grouped_df <- function(x, ...) {
  as_forest.rowwise_df(x, ...)
}

#' Constructs a forest by one or more variables
#'
#' `forest_by()` constructs a forest by one or more variables.
#'
#' @param .data A data frame.
#' @param ... Variables.
#'
#' @return A forest.
#'
#' @export
forest_by <- function(.data, ...) {
  as_forest(dplyr::rowwise(.data, ...))
}
