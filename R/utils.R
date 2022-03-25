big_mark <- function(x) {
  mark <- if (identical(getOption("OutDec"), ",")) {
    "."
  } else {
    ","
  }
  formatC(x, big.mark = mark)
}

plural <- function(x, size) {
  if (size != 1L) {
    x <- paste0(x, "s")
  }
  x
}

commas <- function(...) {
  paste0(...,
         collapse = ", ")
}

drop_cols <- function(x, cols) {
  x[!names(x) %in% cols]
}

drop_node <- function(x) {
  drop_cols(x, "node")
}

cbind_check <- function(...) {
  vec_cbind(...,
            .name_repair = "check_unique")
}

rbind_check <- function(...) {
  vec_rbind(...,
            .name_repair = "check_unique")
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
