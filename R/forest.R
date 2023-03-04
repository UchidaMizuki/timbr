forest <- function(roots, nodes) {
  structure(list(roots = roots,
                 nodes = nodes),
            class = "forest")
}

#' Test if an object is a forest
#'
#' @param x An object.
#'
#' @return `TRUE` if an object inherits from `forest` class.
#'
#' @export
is_forest <- function(x) {
  inherits(x, "forest")
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

#' @rdname as_forest
#' @export
as_forest.rowwise_df <- function(x, ...) {
  grp_vars <- dplyr::group_vars(x)
  x <- ungroup(x)

  grps <- x[grp_vars]
  grps <- vec_cast(grps,
                   tibble::new_tibble(vec_cast_common(!!!grps)))
  x <- x[!names(x) %in% grp_vars]
  stopifnot(
    !vec_duplicate_any(grps)
  )

  # roots
  size_grp_vars <- vec_size(grp_vars)
  roots <- cbind_check(grps[-size_grp_vars],
                       . = vec_seq_along(x))
  roots <- dplyr::grouped_df(roots, grp_vars[-size_grp_vars])

  # nodes
  node <- tibble::new_tibble(df_list(name = grp_vars[[size_grp_vars]],
                                     value = grps[[size_grp_vars]],
                                     parent = NA_integer_))
  nodes <- cbind_check(. = node,
                       x)

  forest(roots, nodes)
}

#' @rdname as_forest
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



# Printing ----------------------------------------------------------------

#' @export
format.forest <- function(x, ...) {
  roots <- x$roots
  nodes <- x$nodes

  root_nodes <- vec_slice(nodes, roots$.)

  if (dplyr::is_grouped_df(roots)) {
    group_sum <- tbl_sum(roots)[2]
  } else {
    group_sum <- NULL
  }

  # roots
  roots$. <- timbr_node(root_nodes$.$name, root_nodes$.$value)

  root_nodes <- drop_node(root_nodes)
  roots <- cbind_check(roots,
                       root_nodes)
  roots <- new_data_frame(roots,
                          size_nodes = vec_size(nodes),
                          size_features = ncol(root_nodes),
                          group_sum = group_sum,
                          tree = tree_forest(x),
                          is_rowwise = is_rowwise_forest(x), ...,
                          class = c("tbl_forest", "tbl"))
  format(roots)
}

#' @importFrom pillar tbl_sum
#' @export
tbl_sum.tbl_forest <- function(x) {
  size_nodes <- attr(x, "size_nodes")
  size_features <- attr(x, "size_features")

  node_names <- field(x$., "name")
  size_rle <- rle(node_names)$lengths

  out <- c(`A forest` = paste(big_mark(size_nodes), plural("node", size_nodes), "and",
                              big_mark(size_features), plural("feature", size_features)),
           attr(x, "group_sum"))
  # Roots = commas(paste0(node_names[cumsum(size_rle)], " [", big_mark(size_rle), "]")))

  if (attr(x, "is_rowwise")) {
    out <- c(out,
             Rowwise = "")
  }

  out
}

#' @importFrom pillar tbl_format_header
#' @export
tbl_format_header.tbl_forest <- function(x, setup, ...) {
  out <- NextMethod()
  trees <- c(pillar::style_subtle("# Trees:"),
             pillar::style_subtle(paste0("#   ", attr(x, "tree"))))

  if (attr(x, "is_rowwise")) {
    size <- vec_size(out)
    out <- c(out[-size],
             trees,
             out[[size]])
  } else {
    out <- c(out, trees)
  }

  out
}

#' @export
print.forest <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}

tree_forest <- function(x) {
  style <- box_chars()

  node_names <- x$nodes$.$name
  group_rle <- vec_group_rle(node_names)
  seq_along <- vec_seq_along(group_rle)
  node_ids <- vec_rep_each(seq_along, field(group_rle, "length"))

  node_parents <- vec_slice(node_ids, x$nodes$.$parent)

  loc <- vec_match(seq_along, node_ids)

  node_names <- vec_slice(node_names, loc)
  node_parents <- vec_slice(node_parents, loc)

  out <- tibble::tibble(. = tibble::tibble(name = node_names,
                                           value = NA_character_,
                                           parent = node_parents),
                        name = node_names,
                        size = field(group_rle, "length"),
                        children = list(character()))
  out <- map_forest_impl(out,
                         function(x, y) {
                           is_last <- vec_seq_along(y) == vec_size(y)
                           children <- purrr::pmap(list(y$name, y$size, y$children, is_last),
                                                   function(name, size, children, is_last) {
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
                           x$children <- list(purrr::list_c(children))
                           x
                         })
  out <- vec_slice(out, is.na(out$.$parent))
  out <- purrr::pmap(list(out$name, out$size, out$children),
                     function(name, size, children) {
                       c(paste0(name, " [", big_mark(size), "]"),
                         children)
                     })
  list_unchop(out)
}
