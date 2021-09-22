#' observed
#'
#' @export
observed <- function (model, condition) {
  condition <- rlang::enexpr(condition)
  nmodel <- model

  nmodel$query <- list()

  if (length(condition) > 2){
    # Method with operator
    # Here we admit that a condition is like: tag operator value
    # e.g. Temperature > 38.5

    operator <- condition[[1]]
    if (is.symbol(condition[[3]])) {
      nmodel$query$tag <- rlang::as_string(condition[[3]])[1]
      wanted_value <- condition[[2]]
      nmodel$measures <- subset(nmodel$measures, tag == nmodel$query$tag)
      nmodel$measures <- nmodel$measures[eval(rlang::call2(operator, wanted_value, nmodel$measures$value)),]

    } else {
      nmodel$query$tag <- rlang::as_string(condition[[2]])[1]
      wanted_value <- condition[[3]]
      nmodel$measures <- subset(nmodel$measures, tag == nmodel$query$tag)
      nmodel$measures <- nmodel$measures[eval(rlang::call2(operator, nmodel$measures$value, wanted_value)),]
    }

  } else {
    # Method without operator
    # When there is no operator, check events or description, measures with description (damn hard)


    nmodel$query$tag <- rlang::as_string(condition)

    nmodel$events <- subset(nmodel$events, tag == nmodel$query$tag)
    nmodel$periods <- subset(nmodel$periods, tag == nmodel$query$tag)

    # TODO: to check description (check on description)

  }
  nmodel
}
