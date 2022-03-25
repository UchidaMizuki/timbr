#' dplyr methods for forest objects
#'
#' dplyr methods for forest objects.
#'
#' @param .data A forest.
#' @param ... Other arguments.
#' @param .node `NULL` (default) or a vector to create new nodes.
#' @param x A forest.
#' @param y A data frame.
#' @param by An unnamed character vector giving the key columns.
#'
#' @return A forest.
#'
#' @name dplyr
NULL

#' @rdname dplyr
#' @importFrom dplyr mutate
#' @export
mutate.forest <- function(.data, ...) {
  roots <- .data$roots
  nodes <- .data$nodes

  grp_vars <- group_vars(roots)

  node_data <- drop_node(nodes)
  root_node_data <- vec_slice(node_data, roots$node)
  root_node_data <- cbind_check(roots[grp_vars],
                                root_node_data)
  root_node_data <- dplyr::new_grouped_df(root_node_data, group_data(roots))

  new_root_node_data <- mutate(root_node_data, ...)
  new_root_node_data <- drop_cols(new_root_node_data, grp_vars)

  new_node_data <- cbind_check(node_data,
                               vec_init(drop_cols(new_root_node_data, names(node_data))))
  new_node_data <- new_node_data[names(new_root_node_data)]

  vec_slice(new_node_data, roots$node) <- new_root_node_data

  .data$nodes <- cbind_check(node = nodes$node,
                             new_node_data)
  .data
}

#' @rdname dplyr
#' @importFrom dplyr summarise
#' @export
summarise.forest <- function(.data, ...,
                             .node = NULL) {
  roots <- .data$roots
  nodes <- .data$nodes

  if (!is.null(.node)) {
    if (!rlang::is_named(.node)) {
      names(.node) <- .node
    }

    stopifnot(
      !names(.node) %in% c(names(roots), nodes$node$name)
    )
  }

  # roots
  grp_keys <- group_keys(roots)
  grp_vars <- names(grp_keys)
  ncol_grp_keys <- ncol(grp_keys)

  new_root_nodes <- vec_size(nodes) + vec_seq_along(grp_keys)

  if (is.null(.node)) {
    new_roots <- cbind_check(grp_keys[-ncol_grp_keys],
                             node = new_root_nodes)
    new_roots <- dplyr::grouped_df(new_roots, grp_vars[-ncol_grp_keys])
  } else {
    new_roots <- cbind_check(grp_keys,
                             node = new_root_nodes)
    new_roots <- dplyr::grouped_df(new_roots, grp_vars)
  }

  # nodes
  # set parents
  vec_slice(nodes$node$parent, roots$node) <- vec_unchop(vec_chop(new_root_nodes),
                                                         group_rows(roots))

  # new nodes
  if (is.null(.node)) {
    new_root_nodes <- tibble::new_tibble(df_list(name = grp_vars[[ncol_grp_keys]],
                                                 value = grp_keys[[ncol_grp_keys]],
                                                 parent = NA_integer_))
  } else {
    new_root_nodes <- tibble::new_tibble(df_list(name = names(.node),
                                                 value = unname(.node),
                                                 parent = NA_integer_))
  }

  # summarise
  root_node_data <- vec_slice(drop_node(nodes), roots$node)
  root_node_data <- cbind_check(roots[grp_vars], root_node_data)
  root_node_data <- dplyr::new_grouped_df(root_node_data, group_data(roots))

  new_root_node_data <- summarise(root_node_data, ...,
                                  .groups = "drop")

  new_root_nodes <- cbind_check(node = new_root_nodes,
                                drop_cols(new_root_node_data, grp_vars))
  new_nodes <- rbind_check(nodes,
                           new_root_nodes)

  forest(new_roots, new_nodes)
}

#' @rdname dplyr
#' @importFrom dplyr select
#' @export
select.forest <- function(.data, ...) {
  modify_nodes(select)(.data, ...)
}

#' @rdname dplyr
#' @importFrom dplyr relocate
#' @export
relocate.forest <- function(.data, ...) {
  modify_nodes(relocate)(.data, ...)
}

modify_nodes <- function(f) {
  function(x, ...) {
    nodes <- x$nodes

    node_data <- drop_node(nodes)
    new_node_data <- f(node_data, ...)

    x$nodes <- cbind_check(node = nodes$node,
                           new_node_data)
    x
  }
}

#' @rdname dplyr
#' @importFrom dplyr rows_update
#' @export
rows_update.forest <- function(x, y,
                               by = NULL, ...) {
  rows_update_forest(x, y,
                     by = by,
                     patch = FALSE)
}

#' @rdname dplyr
#' @importFrom dplyr rows_patch
#' @export
rows_patch.forest <- function(x, y,
                               by = NULL, ...) {
  rows_update_forest(x, y,
                     by = by,
                     patch = TRUE)
}

rows_update_forest <- function(x, y, by, patch) {
  by <- timbr_common_by(by, x, y)
  by_roots <- by$by_roots
  by_nodes <- by$by_nodes

  y_roots <- y[by_roots]
  y_nodes <- y[by_nodes]
  y <- y[!names(y) %in% c(by_roots, by_nodes)]

  # roots
  roots <- x$roots
  root_nodes <- roots$node
  grps <- vec_group_loc(as.data.frame(y_roots))

  y_locs <- vec_slice(grps$loc,
                      vec_match(roots[by_roots], grps$key))

  # nodes
  nodes <- x$nodes
  locs <- timbr_match(nodes$node, root_nodes, y_nodes, y_locs)

  # new_data
  new_data <- vec_slice(y, locs$haystacks)

  if (patch) {
    new_data <- mapply(vec_slice(nodes[names(y)], locs$needles), new_data,
                       FUN = dplyr::coalesce,
                       SIMPLIFY = FALSE)
    new_data <- as_tibble(new_data)
  }

  vec_slice(nodes[names(y)], locs$needles) <- new_data
  x$nodes <- nodes
  x
}

timbr_common_by <- function(by = NULL,
                            x, y) {
  x_root_names <- names(drop_node(x$roots))
  x_node_names <- rev(vec_unique(x$nodes$node$name))

  y_names <- names(y)

  if (is.null(by)) {
    by_roots <- intersect(x_root_names, y_names)
    by_nodes <- intersect(x_node_names, y_names)
    by <- c(by_roots, by_nodes)

    rlang::inform(auto_by_msg(by))
  } else {
    vec_assert(by, character())

    stopifnot(
      all(by %in% c(x_root_names, x_node_names)),
      all(by %in% y_names)
    )

    by_roots <- intersect(by, x_root_names)
    by_nodes <- intersect(by, x_node_names)
  }

  stopifnot(
    !vec_is_empty(by_nodes),
    !vec_duplicate_any(y[by])
  )

  list(by_roots = by_roots,
       by_nodes = by_nodes)
}

timbr_match <- function(needles, needle_locs, haystacks, haystack_locs) {
  ncol_haystacks <- ncol(haystacks)
  haystack_name <- names(haystacks)[[1L]]
  haystack <- vec_chop(haystacks[[1L]], haystack_locs)
  new_haystacks <- haystacks[-1L]

  size <- vec_size(needle_locs)
  out <- vec_init(list(), size)

  for (i in seq_len(size)) {
    needle_loc <- needle_locs[[i]]
    needle <- vec_slice(needles, needle_loc)

    if (needle$name == haystack_name) {
      haystack_loc <- haystack_locs[[i]]

      if (vec_is_empty(haystack_loc)) {
        new_haystack_locs <- integer()
      } else {
        new_haystack_locs <- vec_slice(haystack_loc,
                                       haystack[[i]] == needle$value)
      }

      if (ncol_haystacks == 1L) {
        out[[i]] <- data_frame(needles = needle_loc,
                               haystacks = new_haystack_locs)
      } else {
        new_needle_locs <- which(needles$parent == needle_loc)

        if (!vec_is_empty(new_needle_locs)) {
          new_haystack_locs <- vec_rep(list(new_haystack_locs),
                                       vec_size(new_needle_locs))

          out[[i]] <- timbr_match(needles = needles,
                                  needle_locs = new_needle_locs,
                                  haystacks = new_haystacks,
                                  haystack_locs = new_haystack_locs)
        }
      }
    } else {
      new_needle_locs <- which(needles$parent == needle_loc)
      new_haystack_locs <- vec_rep(list(haystack_locs[[i]]),
                                   vec_size(new_needle_locs))

      out[[i]] <- timbr_match(needles = needles,
                              needle_locs = new_needle_locs,
                              haystacks = haystacks,
                              haystack_locs = new_haystack_locs)
    }
  }
  vec_c(!!!out)
}
