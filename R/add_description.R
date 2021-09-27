
stat_unit_from_hash <- function(stat_units) {
  subset(analysr_env$stat_units, stat_unit == stat_units)[["hash"]]
}


#' add_description
#'
#' @export
add_description <- function (stat_units, label) {
  n <- length(stat_units)
  hash <- stat_unit_from_hash(stat_units)
  result <- data.frame(hash = hash, type = rep(label, n), value = TRUE)
  analysr_env$descriptions <- rbind(analysr_env$descriptions, result)
}
