#' observed
#'
#' @export
observed <- function (model, condition) {
  condition <- rlang::enexpr(condition)

  model$query <- list() # here we reset the query
  model$query$condition <- condition

  if (length(condition) > 2){
    # Method with operator
    # Here we admit that a condition is like: tag operator value
    # e.g. Temperature > 38.5

    #if there's an operator, the information will be in the measure table

    operator <- condition[[1]]
    if (is.symbol(condition[[3]])) {
      model$query$tag <- rlang::as_string(condition[[3]])[1]
      #let's select the stat_units that have the query condition
      # the list will be in stocked in query$stat_units_selected
      nmodel <- model
      tag_to_check <- condition[[3]]
      rvalue <- condition [[2]]
      nmodel <- subset(model$measures, tag == tag_to_check)
      nmodel <- nmodel[eval(rlang::call2(operator, nmodel$value, rvalue)),]
      stat_unit <- nmodel$stat_unit
      date <- nmodel$date
      model$selection <- data.frame(stat_unit, date)
    }

    else {
      model$query$tag <- rlang::as_string(condition[[2]])[1]
      #let's select the stat_units that have the query condition
      # the list will be in stocked in query$stat_units_selected
      nmodel <- model
      tag_to_check <- condition[[2]]
      rvalue <- condition [[3]]
      nmodel <- subset(model$measures, tag == tag_to_check)
      nmodel <- nmodel[eval(rlang::call2(operator, nmodel$value, rvalue)),]
      stat_unit <- nmodel$stat_unit
      date <- nmodel$date
      model$selection <- data.frame(stat_unit, date)

    }

  }


  else {
    # Method without operator
    # When there is no operator, check events or description, measures with description (damn hard)

    model$query$tag <- rlang::as_string(condition)


  }
  model
}
