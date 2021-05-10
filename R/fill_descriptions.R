#' Fill descriptions
#'
#' A function to fill descriptions table when importing data
#' (typically those which are not in mandatory)
#'
#' @param hash a vector containing hash to associate
#' @param types a vector of strings (the name of the columns to import)
#' @param data the data frame you want to extract data from
#' @param n (optional) length of hash if already calculated
fill_descriptions <- function(hash, types, data, n = length(hash)) {

  for (type in types) {
    result <- data.frame(hash = hash, type = rep(type, n), value = data[, type])
    analysr_env$descriptions <- rbind(analysr_env$descriptions, result)
  }
}
