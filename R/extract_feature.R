#' extract_feature
#'
#' @param model An AnalysR model
#' @param tag A tag existing in the description table
#'
#' @export
extract_feature <- function(model, tag) {

  temp <- dplyr::inner_join(model$stat_units, model$descriptions, by = "hash")
  temp <- subset(temp, type == tag)
  temp <- tibble::tibble(stat_unit = temp$stat_unit, x = temp$value)
  colnames(temp) <- c("stat_unit", tag)
  model$stat_units <- dplyr::left_join(model$stat_units, temp, by = "stat_unit")

  model

}
