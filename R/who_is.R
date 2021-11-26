

#' who_is
#'
#' @name who_is
#'
#' @param model An AnalysR model or a list of stat_unit id
#' @param condition A condition.
#'
#' @export
who_is <- function(model, condition) {
  condition <- rlang::enexpr(condition)
  selection <- tibble::tibble()
  if (length(condition) > 2) {
    # Method with operator
    # Here we admit that a condition is like: tag operator value
    # e.g. Temperature > 38.5

    # if there's an operator, the information will be in the measure table

    operator <- condition[[1]]
    if (is.symbol(condition[[3]])) {
      # let's select the stat_units that have the query condition
      # the list will be in stocked in query$stat_units_selected
      tag_to_check <- condition[[3]]
      rvalue <- condition[[2]]

      # Check on descriptions table
      temp <- subset(model$descriptions, type == tag_to_check)
      temp <- temp[eval(rlang::call2(
        operator, rvalue,
        convert_to_best_type(temp$value)
      )), ]
      stat_unit <- stat_unit_from_hash(temp$hash)
      selection <- rbind(selection, tibble::tibble(stat_unit))
    } else {
      tag_to_check <- condition[[2]]
      rvalue <- condition[[3]]

      # Check on descriptions table
      temp <- subset(model$descriptions, type == tag_to_check)
      temp <- temp[eval(rlang::call2(
        operator,
        convert_to_best_type(temp$value), rvalue
      )), ]

      stat_unit <- stat_unit_from_hash(temp$hash)
      selection <- rbind(selection, tibble::tibble(stat_unit))
    }
  } else {
    # Method without operator
    # When there is no operator, check events or description,
    # measures with description (damn hard)

    # Check on descriptions table
    temp <- subset(model$descriptions, type == tag_to_check)
    stat_unit <- stat_unit_from_hash(temp$hash)
    selection <- rbind(selection, tibble::tibble(stat_unit))
  }

  model$selection <- merge(model$selection, selection, by = "stat_unit")

  return(model)
}
