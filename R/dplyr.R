#' @export
summarise.timbr_forest <- function(.data, ...,
                                   .node = NULL) {
  if (!is.null(.node)) {
    if (!rlang::is_named(.node)) {
      .node <- rlang::set_names(.node)
    }
  }

  roots <- .data$roots
  graph <- .data$graph

  group_keys <- dplyr::group_keys(roots)
  group_rows <- dplyr::group_rows(roots)
  group_vars <- names(group_keys)
  size_group_vars <- vec_size(group_vars)

  nodes <- get_nodes(.data)
  new_root_nodes <- vec_size(nodes) + vec_seq_along(group_keys)

  if (is.null(.node)) {
    new_roots <- data_frame(group_keys[-size_group_vars],
                            . = new_root_nodes) |>
      dplyr::grouped_df(group_vars[-size_group_vars])
  } else {
    new_roots <- data_frame(group_keys,
                            . = new_root_nodes) |>
      dplyr::grouped_df(group_vars)
  }

  nodes <- data_frame(roots[group_vars],
                      vec_slice(nodes, roots$.)) |>
    dplyr::new_grouped_df(dplyr::group_data(roots)) |>
    dplyr::summarise(...,
                     .groups = "drop") |>
    dplyr::select(!dplyr::any_of(group_vars))
  if (is.null(.node)) {
    nodes <- data_frame(. = node(name = group_vars[[size_group_vars]],
                                 value = group_keys[[size_group_vars]]),
                        nodes)
  } else {
    nodes <- data_frame(. = node(name = names(.node),
                                 value = unname(.node)),
                        nodes)
  }

  edges <- data_frame(from = vec_rep_each(new_root_nodes, list_sizes(group_rows)),
                      to = roots$.[list_unchop(group_rows)]) |>
    dplyr::arrange(.data$to)

  .data$roots <- new_roots
  .data$graph <- graph |>
    tidygraph::bind_nodes(nodes) |>
    tidygraph::bind_edges(edges)
  .data
}

#' @export
mutate.timbr_forest <- function(.data, ...) {
  roots <- .data$roots

  root_nodes <- get_root_nodes(.data) |>
    dplyr::mutate(...) |>
    dplyr::ungroup() |>
    dplyr::select(!dplyr::any_of(names(roots)))

  .data$graph <- .data$graph |>
    tidygraph::activate("nodes") |>
    quiet_focus(dplyr::row_number() %in% roots$.) |>
    dplyr::mutate(root_nodes) |>
    tidygraph::unfocus()
  .data
}

#' @export
select.timbr_forest <- function(.data, ...) {
  modify_nodes(dplyr::select)(.data, ...)
}
