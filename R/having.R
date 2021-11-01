

#' @name having
#'
#' @param rmodel An AnalysR model or a list of stat_unit id
#' @param condition A condition.
#'
#' @export
having <- function(model, condition) {
  condition <- rlang::enexpr(condition)
  selection <- data.frame()
  # check if the input is a vector or an AnalysR env
  if (is.vector(model)) {
    rmodel <- analysr_env
  } else {
    rmodel <- model
  }
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
      temp <- subset(rmodel$descriptions, type == tag_to_check)
      temp <- temp[eval(rlang::call2(
        operator, rvalue,
        convert_to_best_type(temp$value)
      )), ]
      stat_unit <- stat_unit_from_hash(temp$hash)
      selection <- rbind(selection, data.frame(stat_unit))
    } else {
      tag_to_check <- condition[[2]]
      rvalue <- condition[[3]]

      # Check on descriptions table
      temp <- subset(rmodel$descriptions, type == tag_to_check)
      temp <- temp[eval(rlang::call2(
        operator,
        convert_to_best_type(temp$value), rvalue
      )), ]

      stat_unit <- stat_unit_from_hash(temp$hash)
      selection <- rbind(selection, data.frame(stat_unit))
    }
  } else {
    # Method without operator
    # When there is no operator, check events or description,
    # measures with description (damn hard)

    # Check on descriptions table
    temp <- subset(rmodel$descriptions, type == tag_to_check)
    stat_unit <- stat_unit_from_hash(temp$hash)
    selection <- rbind(selection, data.frame(stat_unit))
  }

  # check if the input is a vector or an AnalysR env
  if (!is.vector(model)) {
    rmodel$selection <- merge(rmodel$selection, selection, by = "stat_unit")

    return(rmodel)
  } else {
    return(intersect(selection$stat_unit, model))
  }
}