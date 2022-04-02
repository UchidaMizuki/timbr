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
  x <- dplyr::ungroup(x)

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

#' @export
rbind.forest <- function(..., deparse.level = 1) {
  frs <- rlang::list2(...)
  size_frs <- vec_size(frs)

  new_roots <- vec_init(list(), size_frs)
  new_nodes <- vec_init(list(), size_frs)

  size_nodes <- 0L
  for (i in seq_len(size_frs)) {
    fr <- frs[[i]]
    roots <- fr$roots
    nodes <- fr$nodes

    roots$. <- roots$. + size_nodes
    new_roots[[i]] <- roots

    nodes$.$parent <- nodes$.$parent + size_nodes
    new_nodes[[i]] <- nodes

    size_nodes <- size_nodes + vec_size(nodes)
  }

  new_roots <- rbind_check(!!!new_roots)
  loc <- which(names(new_roots) == ".")
  new_roots <- new_roots[c(vec_as_location(-loc, ncol(new_roots)), loc)]

  new_nodes <- rbind_check(!!!new_nodes)
  stopifnot(
    vec_is_empty(intersect(names(drop_node(new_roots)),
                           names(drop_node(new_nodes))))
  )

  forest(new_roots, new_nodes)
}



# Verbs -------------------------------------------------------------------

#' Children of the forest
#'
#' Convert a forest into a forest consisting of its child nodes.
#'
#' @param data A forest.
#' @param name `NULL` (default) or a scalar character specifying the node name
#' of child nodes.
#'
#' @return A forest.
#'
#' @export
children <- function(data,
                     name = NULL) {
  name <- rlang::enquo(name)

  if (rlang::quo_is_null(name)) {
    name <- vec_slice(data$nodes$.$name, data$roots$.)
    name <- vec_unique(name)
    stopifnot(
      rlang::is_scalar_character(name)
    )
  } else {
    name <- rlang::as_name(name)
  }

  data <- timbr_pull(data, name)
  timbr_children(data, name)
}

timbr_children <- function(data,
                           name = NULL) {
  roots <- data$roots
  nodes <- data$nodes

  new_root_keys <- drop_node(roots)

  if (!is.null(name)) {
    new_root_keys <- cbind_check(new_root_keys,
                                 !!name := vec_slice(nodes$.$value, roots$.))
  }

  # new_nodes
  new_root_locs <- vec_in(nodes$.$parent, roots$.)
  new_root_nodes <- vec_slice(nodes, new_root_locs)

  new_root_keys <- vec_slice(new_root_keys,
                             vec_match(new_root_nodes$.$parent, roots$.))

  new_root_nodes$.$parent <- NA_integer_
  vec_slice(nodes, new_root_locs) <- new_root_nodes

  node_locs <- vec_as_location(-roots$., vec_size(nodes))
  new_nodes <- vec_slice(nodes, node_locs)
  new_node_locs <- vec_seq_along(new_nodes)
  new_nodes$.$parent <- new_nodes$.$parent + new_node_locs - node_locs

  # new_roots
  new_roots <- cbind_check(new_root_keys,
                           . = vec_slice(new_node_locs,
                                         vec_equal_na(new_nodes$.$parent)))
  new_roots <- dplyr::grouped_df(new_roots, names(new_root_keys))

  forest(new_roots, new_nodes)
}

#' Climb a forest from parents to children
#'
#' Climb a forest from parents to children with one or more node names.
#'
#' @param .data A forest.
#' @param ... A list of node names to climb the forest.
#' @param .deep Whether to search deeply for node names or not?
#'
#' @return A forest.
#'
#' @export
climb <- function(.data, ...,
                  .deep = TRUE) {
  names <- rlang::enquos(...)

  if (vec_is_empty(names)) {
    .data
  } else {
    name <- rlang::as_name(names[[1L]])
    names <- names[-1L]

    if (.deep) {
      nodes <- .data$nodes
      root_nodes <- vec_slice(nodes, .data$roots$.)
      root_node_names <- vec_unique(root_nodes$.$name)

      frs <- vec_init_along(list(), root_node_names)

      for (i in vec_seq_along(root_node_names)) {
        root_node_name <- root_node_names[[i]]
        fr <- timbr_pull(.data, root_node_name)

        if (root_node_name == name) {
          if (vec_is_empty(names)) {
            frs[[i]] <- fr
          } else {
            fr <- timbr_children(fr, name)
            frs[[i]] <- climb(fr, !!!names)
          }
        } else {
          fr <- timbr_children(fr)

          if (vec_is_empty(fr$nodes)) {
            frs[[i]] <- fr
          } else {
            frs[[i]] <- climb(fr, !!name, !!!names)
          }
        }
      }

      rlang::exec(rbind, !!!frs)
    } else {
      out <- timbr_pull(.data, name)

      if (!vec_is_empty(names)) {
        out <- timbr_children(out, name)
        climb(out, !!!names,
              .deep = FALSE)
      }
    }
  }
}

timbr_pull <- function(data, name) {
  roots <- data$roots
  nodes <- data$nodes
  root_nodes <- vec_slice(nodes, roots$.)

  name <- tidyselect::vars_pull(vec_unique(root_nodes$.$name), name)

  locs <- vec_equal(root_nodes$.$name, name,
                    na_equal = TRUE)
  new_roots <- vec_slice(roots, locs)
  new_root_nodes <- new_roots$.

  grps <- vec_group_loc(nodes$.$parent)
  grps <- vec_slice(grps, !is.na(grps$key))
  grp_keys <- grps$key

  node_locs <- integer()
  repeat {
    node_locs <- vec_c(new_root_nodes, node_locs)
    root_grps <- vec_slice(grps, vec_in(grp_keys, new_root_nodes))
    new_root_nodes <- vec_c(!!!root_grps$loc)
    if (vec_is_empty(new_root_nodes)) {
      break
    }
  }

  # new_nodes
  node_locs <- vec_sort(node_locs)
  new_node_locs <- vec_seq_along(node_locs)

  new_nodes <- vec_slice(nodes, node_locs)
  new_nodes$.$parent <- new_nodes$.$parent + new_node_locs - node_locs

  # new_roots
  new_root_keys <- drop_node(new_roots)
  new_roots <- cbind_check(new_root_keys,
                           . = vec_slice(new_node_locs, vec_equal_na(new_nodes$.$parent)))

  if (dplyr::is_grouped_df(new_root_keys)) {
    new_roots <- dplyr::new_grouped_df(new_roots, group_data(new_root_keys))
  }

  forest(new_roots, new_nodes)
}



# Grouping ----------------------------------------------------------------

#' @importFrom dplyr group_data
#' @export
group_data.forest <- function(.data) {
  modify_roots(group_data)(.data)
}

#' @importFrom dplyr group_keys
#' @export
group_keys.forest <- function(.tbl, ...) {
  modify_roots(group_keys)(.tbl, ...)
}

#' @importFrom dplyr group_indices
#' @export
group_indices.forest <- function(.data, ...) {
  modify_roots(group_indices)(.data, ...)
}

#' @importFrom dplyr group_vars
#' @export
group_vars.forest <- function(x) {
  modify_roots(group_vars)(x)
}

#' @importFrom dplyr groups
#' @export
groups.forest <- function(x) {
  modify_roots(groups)(x)
}

#' @importFrom dplyr group_size
#' @export
group_size.forest <- function(x) {
  modify_roots(group_size)(x)
}

#' @importFrom dplyr n_groups
#' @export
n_groups.forest <- function(x) {
  modify_roots(n_groups)(x)
}

modify_roots <- function(f) {
  function(x, ...) {
    f(x$roots, ...)
  }
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
                          group_sum = group_sum, ...,
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

  c(`A forest` = paste(big_mark(size_nodes), plural("node", size_nodes), "and",
                       big_mark(size_features), plural("feature", size_features)),
    attr(x, "group_sum"),
    Roots = commas(paste0(node_names[cumsum(size_rle)], " [", big_mark(size_rle), "]")))
}

#' @export
print.forest <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}
