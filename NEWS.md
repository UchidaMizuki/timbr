# timbr (development version)

* New "Introduction to timbr" article on the package website walks through creating forests, hierarchical aggregation, combining forests, and `traverse()`.

# timbr 0.3.0

* Fixed a bug where forests with grouped root nodes (e.g. those produced by `summarise()`) could error or behave incorrectly in downstream operations.
* `children()` now gives a clear tidyselect error when a forest's root nodes don't share a single node name, instead of an internal assertion failure.
* `climb()`'s `.deep` argument has been renamed to `.recurse` (`.deep` is now deprecated), and a recursion bug that could corrupt node bookkeeping when climbing multiple levels has been fixed (#9).
* `leaves()` now correctly returns forests where every node is both root and leaf (e.g. single-level trees), instead of erroring.
* `map_forest()` is now defunct and throws an error; use `traverse()` instead (it has been deprecated since timbr 0.2.2).
* `relocate()` is now implemented for forests, moving columns while preserving the tree structure.
* `rows_patch()` and `rows_update()` are now implemented for forests, with fixes to `by`-column matching and error messages (#20).
* `select()` for forests now always preserves the internal node column, so selecting feature columns no longer breaks the tree structure.

# timbr 0.2.2

* Rename `map_forest()` to `traverse` and deprecate `map_forest()`.
* Update behavior of `as_tbl_graph.forest()`.
* Update deprecated functions in vctrs.

# timbr 0.2.1

* Rename the argument for `rowwise.forest()`.
* Fix problems with CRAN checks.

# timbr 0.2.0

* Now, display the tree structure in the header of `forest`.
* Fix problems with CRAN checks.

# timbr 0.1.1

* Rename modify.forest() to map_forest() because modify() will no longer be S3 
generics in purrr 1.0.0 (#1).

# timbr 0.1.0

* This is a new release.
