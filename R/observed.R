

observed <- function (model, condition) {
  condition <- rlang::enexpr(condition)
  nmodel <- model

  if (length(condition) > 2){
    # Method with operator
    # Here we admit that a condition is like: tag operator value
    # e.g. Temperature > 38.5

    operator <- condition[[1]]
    if (is.symbol(condition[[3]])) {
      tag_to_check <- rlang::as_string(condition[[3]])[1]
      wanted_value <- condition[[2]]
      nmodel$measures <- subset(nmodel$measures, tag == tag_to_check)
      nmodel$measures <- nmodel$measures[eval(rlang::call2(operator, wanted_value, nmodel$measures$value)),]

    } else {
      tag_to_check <- rlang::as_string(condition[[2]])[1]
      wanted_value <- condition[[3]]
      nmodel$measures <- subset(nmodel$measures, tag == tag_to_check)
      nmodel$measures <- nmodel$measures[eval(rlang::call2(operator, nmodel$measures$value, wanted_value)),]
    }

  } else {
    # Method without operator
    # When there is no operator, check events or description, measures with description (damn hard)


    tag_to_check <- rlang::as_string(condition)

    nmodel$events <- subset(nmodel$events, tag == tag_to_check)
    nmodel$periods <- subset(nmodel$periods, tag == tag_to_check)

    # TODO: to check description (check on description)

  }
  nmodel
}
