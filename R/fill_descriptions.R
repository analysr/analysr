#' Fill descriptions
#'
#' A function to fill descriptions table when importing data 
#' (typically those which are not in mandatory)
#'
#' @param hash a vector containing hash to associate
#' @param type a string, the name of the column to import
#' @param data the data frame you want to extract data from
fill_descriptions <- function(hash, type, data) {

  n <- length(hash) # can be optimized

  result <- data.frame(hash = hash, type = rep(type, n), value=data[,type])

  analysr_env$descriptions <- rbind(analysr_env$descriptions, result)
}
