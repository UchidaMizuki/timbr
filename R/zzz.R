.onLoad <- function(...) {
  timbr_match <<- memoise::memoise(timbr_match)
  timbr_climb <<- memoise::memoise(timbr_climb)
  timbr_pull_loc <<- memoise::memoise(timbr_pull_loc)
}
