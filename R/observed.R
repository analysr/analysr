#' observed
#'
#' @export
observed <- function (model, condition) {
  condition <- rlang::enexpr(condition)

  model$query <- list(condition=condition)

  if (length(condition) > 2){
    # Method with operator
    # Here we admit that a condition is like: tag operator value
    # e.g. Temperature > 38.5

    operator <- condition[[1]]
    if (is.symbol(condition[[3]])) {
      model$query$tag <- rlang::as_string(condition[[3]])[1]
    } else {
      model$query$tag <- rlang::as_string(condition[[2]])[1]
    }

  } else {
    # Method without operator
    # When there is no operator, check events or description, measures with description (damn hard)

    model$query$tag <- rlang::as_string(condition)


  }
  model
}
