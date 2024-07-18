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
  tibble::tibble(drop_node(roots),
                 vec_slice(nodes, x$roots$.))
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
    dplyr::select() |>
    dplyr::mutate(id = dplyr::row_number()) |>
    dplyr::filter(tidygraph::node_is_root()) |>
    dplyr::pull("id")
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

auto_by_msg <- function(by) {
  by_quoted <- encodeString(by, quote = "\"")

  if (length(by_quoted) == 1L) {
    by_code <- by_quoted
  } else {
    by_code <- paste0("c(", paste(by_quoted, collapse = ", "), ")")
  }

  paste0("Matching, by = ", by_code)
}
