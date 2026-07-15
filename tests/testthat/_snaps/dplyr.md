# rows_update

    Code
      children(rows_update(fr, df, by = c("key2", "key1")))
    Condition
      Error in `f()`:
      ! All columns in `y` must exist in `x`.
      i The following columns only exist in `y`: `key1`, `key2`, and `value`.

---

    Code
      children(rows_patch(fr, df, by = c("key2", "key1")))
    Condition
      Error in `f()`:
      ! All columns in `y` must exist in `x`.
      i The following columns only exist in `y`: `key1`, `key2`, and `value`.

---

    Code
      children(rows_update(fr, df, by = c("key1", "key2")))
    Condition
      Error in `f()`:
      ! `y` must contain keys that already exist in `x`.
      i The following rows in `y` have keys that don't exist in `x`: `c(3, 6)`.
      i Use `unmatched = "ignore"` if you want to ignore these `y` rows.

---

    Code
      children(rows_patch(fr, df, by = c("key1", "key2")))
    Condition
      Error in `f()`:
      ! `y` must contain keys that already exist in `x`.
      i The following rows in `y` have keys that don't exist in `x`: `c(3, 6)`.
      i Use `unmatched = "ignore"` if you want to ignore these `y` rows.

