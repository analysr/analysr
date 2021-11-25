

#' described_by
#' @name described_by
#'
#' @param model An AnalysR model
#' @param condition A condition.
#'
#' @export
described_by <- function(model, condition) {

  condition <- rlang::enexpr(condition)

#First we select the entries in the DESCRIPTION table that match the wanted
  #condition

  hashs_to_keep <- tibble::tibble()
  if (length(condition) > 2) {
    # Method with operator
    # Here we admit that a condition is like: tag operator value
    # e.g. Temperature > 38.5

    #if there's an operator, the information will be in the measure table

    operator <- condition[[1]]
    if (is.symbol(condition[[3]])) {
      # let's select the stat_units that have the query condition
      # the list will be in stocked in query$stat_units_selected
      tag_to_check <- condition[[3]]
      rvalue <- condition [[2]]

      # Check on descriptions table
      temp <- subset(model$descriptions, type == tag_to_check)
      temp <- temp[eval(rlang::call2(operator, rvalue,
                                     convert_to_best_type(temp$value))),]
      if (nrow(temp) != 0) {
        hashs_to_keep <- rbind(hashs_to_keep, temp)
      }

    } else {

      # let's select the stat_units that have the query condition
      # the list will be in stocked in query$stat_units_selected
      tag_to_check <- condition[[2]]
      rvalue <- condition [[3]]

      # Check on descriptions table
      temp <- subset(model$descriptions, type == tag_to_check)
      temp <- temp[eval(rlang::call2(operator,
                                     convert_to_best_type(temp$value), rvalue)),]

      if (nrow(temp) != 0) {
        hashs_to_keep <- rbind(hashs_to_keep, temp)
      }
    }

  }
  else {
    # Method without operator
    # When there is no operator, check events or description,
    # measures with description (damn hard)

    tag_to_check <- condition

    # Check on descriptions table
    temp <- subset(model$descriptions, type == tag_to_check)
    if (nrow(temp) != 0) {
      hashs_to_keep <- rbind(hashs_to_keep, temp)
    }

  }


#Now let's intersect the data we just selected we the one OBSERVED gave us
  #aka intersection between hashs_to_keep and model$selection


  hashs_to_keep <- hashs_to_keep[,"hash"]

  sel <- merge(model$selection, hashs_to_keep,
               by.x = "hash_obs",
               by.y = "hash")

  model$selection <- rbind(sel, merge(model$selection, hashs_to_keep,
                                      by.x = "hash_stat_unit",
                                      by.y = "hash"))



 model
}
