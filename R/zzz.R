.onLoad <- function(...) {
  timbr_match <<- memoise::memoise(timbr_match)
}
