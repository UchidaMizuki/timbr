# Changelog

## timbr (development version)

- Fixed a bug where forests with grouped root nodes (e.g. those produced
  by
  [`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html))
  could error or behave incorrectly in downstream operations.
- [`children()`](https://uchidamizuki.github.io/timbr/reference/children.md)
  now gives a clear tidyselect error when a forest’s root nodes don’t
  share a single node name, instead of an internal assertion failure.
- [`climb()`](https://uchidamizuki.github.io/timbr/reference/climb.md)’s
  `.deep` argument has been renamed to `.recurse` (`.deep` is now
  deprecated), and a recursion bug that could corrupt node bookkeeping
  when climbing multiple levels has been fixed
  ([\#9](https://github.com/UchidaMizuki/timbr/issues/9)).
- [`leaves()`](https://uchidamizuki.github.io/timbr/reference/leaves.md)
  now correctly returns forests where every node is both root and leaf
  (e.g. single-level trees), instead of erroring.
- [`relocate()`](https://dplyr.tidyverse.org/reference/relocate.html) is
  now implemented for forests, moving columns while preserving the tree
  structure.
- [`rows_patch()`](https://dplyr.tidyverse.org/reference/rows.html) and
  [`rows_update()`](https://dplyr.tidyverse.org/reference/rows.html) are
  now implemented for forests, with fixes to `by`-column matching and
  error messages
  ([\#20](https://github.com/UchidaMizuki/timbr/issues/20)).
- [`select()`](https://dplyr.tidyverse.org/reference/select.html) for
  forests now always preserves the internal node column, so selecting
  feature columns no longer breaks the tree structure.

## timbr 0.2.2

CRAN release: 2023-04-29

- Rename
  [`map_forest()`](https://uchidamizuki.github.io/timbr/reference/map_forest.md)
  to `traverse` and deprecate
  [`map_forest()`](https://uchidamizuki.github.io/timbr/reference/map_forest.md).
- Update behavior of `as_tbl_graph.forest()`.
- Update deprecated functions in vctrs.

## timbr 0.2.1

CRAN release: 2023-04-07

- Rename the argument for `rowwise.forest()`.
- Fix problems with CRAN checks.

## timbr 0.2.0

CRAN release: 2023-03-04

- Now, display the tree structure in the header of `forest`.
- Fix problems with CRAN checks.

## timbr 0.1.1

CRAN release: 2022-09-19

- Rename modify.forest() to map_forest() because modify() will no longer
  be S3 generics in purrr 1.0.0
  ([\#1](https://github.com/UchidaMizuki/timbr/issues/1)).

## timbr 0.1.0

CRAN release: 2022-03-29

- This is a new release.
