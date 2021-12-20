


#' add_description
#'
#' @param input A vector of stats_units to be added or an AnalysR env
#' @param label Label to write in description table
#'
#' @export
add_description <- function(input, label) {
  model <- analysr_env
  if (!is.vector(input)) {
    model <- input
    stat_units <- dplyr::pull(model$selection, stat_unit)
  } else {
    stat_units <- input
  }
  if (model$space_to_underscore) {
      label <- gsub(" ", "_", label)
  }
  hash <- dplyr::filter(model$stat_units, stat_unit %in% stat_units)$hash
  n <- length(hash)
  result <- tibble::tibble(hash = hash, type = rep(label, n), value = TRUE)
  model$descriptions <- rbind(model$descriptions, result)
  model
}
