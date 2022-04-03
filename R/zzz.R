.onLoad <- function(...) {
  timbr_match <<- memoise::memoise(timbr_match)
  timbr_pull_loc <<- memoise::memoise(timbr_pull_loc)
}
