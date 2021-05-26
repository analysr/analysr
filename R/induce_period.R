#' induce_period
#'
#' Create period from measures table depending on a condition,
#' and add them to periods table
#'
#' @return The part of  that have been created.
#'
#' @param condition A condition (to be defined).
#' @param tag_to_create A string for the tag to add.
#' @param duration A duration.
#'
#' @export
induce_period <- function(condition, tag_to_create, duration) {

  condition <- rlang::enexpr(condition)
  duration <- rlang::enexpr(duration)

  duration <- duration[[2]]*eval(rlang::call2("::", "lubridate", duration[[3]]))()

  if (length(condition) > 2){
      # Method with operator
      # Here we admit that a condition is like: tag operator value
      # e.g. Temperature > 37.5

      tag_to_check <- rlang::as_string(condition[[2]])[1]

      operator <- condition[[1]]
      wanted_value <- condition[[3]]

      data <- subset(analysr_env$measures, tag == tag_to_check)
      data <- data[eval(rlang::call2(operator, data$value, wanted_value)),]
  }

  else if (length(condition > 0)){
    # Method without operator

    tag_to_check <- rlang::as_string(toString(condition))

    data <- subset(analysr_env$measures, tag == tag_to_check)


  } else {
    # Method not developed
  }
  n <- nrow(data)

  result <- data.frame(hash = get_hash(n), stat_unit = data$stat_unit,
                       begin = data$date, end = data$date + duration,
                       desc = rep(tag_to_create, n))

  analysr_env$periods <- rbind(analysr_env$periods, result)
}
