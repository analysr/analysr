
hash_from_stat_unit <- function(stat_units) {
  result <- c()
  for (i in rownames(analysr_env$stat_units)) {
    for (j in stat_units) {
      if (analysr_env$stat_units[i,]$stat_unit == j) {
        result <- c(result, analysr_env$stat_units[i,]$hash)
      }
    }
  }
  result
}


#' add_description
#'
#' @param stat_units A vector of stats_units to be added
#' @param label Label to write in description table
#'
#' @export
add_description <- function (stat_units, label) {
  label <- gsub(" ", "_", label) # maybe use global config
  n <- length(stat_units)
  hash <- hash_from_stat_unit(stat_units)
  result <- data.frame(hash = hash, type = rep(label, n), value = TRUE)
  analysr_env$descriptions <- rbind(analysr_env$descriptions, result)
}
