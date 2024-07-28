#' @export
format.timbr_forest <- function(x, ...) {
  setup <- tbl_format_setup(x, ...,
                            n = nrow(x$roots))
  header <- tbl_format_header(x, setup)
  body <- tbl_format_body(x, setup)
  footer <- tbl_format_footer(x, setup)
  c(header, body, footer)
}

#' @export
print.timbr_forest <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}

#' @export
tbl_sum.timbr_forest <- function(x, ...) {
  roots <- x$roots
  nodes <- get_nodes(x)

  if (dplyr::is_grouped_df(roots)) {
    group_sum <- tbl_sum(roots)[2]
  } else {
    group_sum <- NULL
  }

  size_nodes <- vec_size(nodes)
  size_features <- ncol(nodes) - 1L

  node_names <- get_node_name(nodes$.)
  size_rle <- rle(node_names)$lengths

  c(`A forest` = paste(big_mark(size_nodes), plural("node", size_nodes), "and",
                       big_mark(size_features), plural("feature", size_features)),
    group_sum,
    Trees = "",
    tree_forest(x))
}

#' @export
tbl_format_setup.timbr_forest <- function(x, ...) {
  structure(list(x = x,
                 tbl_sum = tbl_sum(x)),
            class = "pillar_tbl_format_setup")
}

#' @export
tbl_format_header.timbr_forest <- function(x, setup, ...) {
  tbl_sum <- setup$tbl_sum
  nms <- names(tbl_sum)
  header <- paste0(dplyr::if_else(nms == "",
                                  "",
                                  pillar::align(paste0(nms, ": "))),
                   dplyr::if_else(nms == "",
                                  paste0("  ", tbl_sum),
                                  tbl_sum))
  pillar::style_subtle(paste0("# ", header))
}

#' @export
tbl_format_body.timbr_forest <- function(x, setup, ...) {
  root_nodes <- get_root_nodes(x)
  tbl_format_body(root_nodes,
                  setup = pillar::tbl_format_setup(root_nodes))
}

#' @export
tbl_format_footer.timbr_forest <- function(x, setup, ...) {
  root_nodes <- get_root_nodes(x)
  tbl_format_footer(root_nodes,
                    setup = pillar::tbl_format_setup(root_nodes))
}

tree_forest <- function(x) {
  style <- box_chars()

  nodes <- get_nodes(x)
  node_names <- get_node_name(nodes$.)

  parent_node_ids <- get_parent_node_ids(x)
  parent_node_names <- vec_slice(node_names, parent_node_ids)
  group_rle <- vec_group_rle(data_frame(node_names = node_names,
                                                      parent_node_names = parent_node_names))
  seq_along <- vec_seq_along(group_rle)
  node_ids <- vec_rep_each(seq_along, field(group_rle, "length"))

  parent_node_ids <- vec_slice(node_ids, parent_node_ids)

  loc <- vec_match(seq_along, node_ids)

  node_names <- vec_slice(node_names, loc)
  parent_node_ids <- vec_slice(parent_node_ids, loc)
  seq_along <- vec_seq_along(node_names)

  nodes <- data_frame(. = node(name = node_names,
                                      value = NA_character_),
                             name = node_names,
                             size = field(group_rle, "length"),
                             children = list(character()))
  edges <- data_frame(from = parent_node_ids,
                             to = seq_along) |>
    vec_slice(!vec_detect_missing(parent_node_ids))
  graph <- tidygraph::tbl_graph(nodes = nodes,
                                edges = edges)

  roots <- data_frame(. = seq_along) |>
    vec_slice(vec_detect_missing(parent_node_ids))

  forest <- new_forest(roots, graph) |>
    traverse(\(x, y) {
      is_last <- vec_seq_along(y) == vec_size(y)
      children <- list(y$name, y$size, y$children, is_last) |>
        purrr::pmap(\(name, size, children, is_last) {
          if (is_last) {
            prefix_head <- paste0(style$l, style$h)
            prefix_tail <- "  "
          } else {
            prefix_head <- paste0(style$j, style$h)
            prefix_tail <- paste0(style$v, " ")
          }

          c(paste0(prefix_head, name, " [", big_mark(size), "]"),
            paste0(prefix_tail, children,
                   recycle0 = TRUE))
        })
      x$children <- list(list_unchop(children))
      x
    })

  root_nodes <- get_root_nodes(forest)
  list(root_nodes$name, root_nodes$size, root_nodes$children) |>
    purrr::pmap(\(name, size, children) {
      c(paste0(name, " [", big_mark(size), "]"),
        children)
    }) |>
    list_unchop()
}
