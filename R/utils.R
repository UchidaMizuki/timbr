get_graph <- function(x) {
  if (tidygraph::is.tbl_graph(x)) {
    x
  } else {
    x$graph
  }
}

get_nodes <- function(x) {
  tibble::as_tibble(tidygraph::activate(get_graph(x), "nodes"))
}

get_edges <- function(x) {
  tibble::as_tibble(tidygraph::activate(get_graph(x), "edges"))
}

get_root_nodes <- function(x) {
  roots <- x$roots
  nodes <- get_nodes(x)
  root_nodes <- tibble::tibble(drop_node(roots),
                               vec_slice(nodes, x$roots$.))
  dplyr::new_grouped_df(root_nodes, dplyr::group_data(x$roots))
}

get_parent_node_ids <- function(x) {
  nodes <- get_nodes(x)
  edges <- get_edges(x)

  vec_slice(edges$from,
            vec_match(vec_seq_along(nodes), edges$to))
}

get_root_node_ids <- function(x) {
  get_graph(x) |>
    tidygraph::activate("nodes") |>
    dplyr::mutate(.rows = dplyr::row_number()) |>
    dplyr::filter(tidygraph::node_is_root()) |>
    dplyr::pull(".rows")
}

drop_node <- function(data) {
  data[names(data) != "."]
}

modify_roots <- function(f) {
  function(x, ...) {
    f(x$roots, ...)
  }
}

modify_nodes <- function(f) {
  function(.data, ...) {
    .data$graph <- .data$graph |>
      tidygraph::activate("nodes") |>
      f(...)
    .data
  }
}

grouped_df_roots <- function(data, roots) {
  root_names <- names(drop_node(data))

  if (vec_is_empty(root_names)) {
    data
  } else {
    dplyr::grouped_df(data, root_names)
  }
}

quiet_focus <- function(.data, ...) {
  .data |>
    purrr::quietly(tidygraph::focus)(...) |>
    purrr::chuck("result")
}

# From: https://github.com/r-lib/cli/blob/main/R/tree.R
box_chars <- function() {
  if (cli::is_utf8_output()) {
    list(h = "\u2500", v = "\u2502", l = "\u2514", j = "\u251C")
  } else {
    list(h = "-", v = "|", l = "\\", j = "+")
  }
}

big_mark <- function(x) {
  mark <- if (identical(getOption("OutDec"), ",")) "." else ","
  formatC(x, big.mark = mark)
}

plural <- function(x, size) {
  if (size != 1L) {
    x <- paste0(x, "s")
  }
  x
}
