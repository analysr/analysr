#' induce_event
#'
#' Create event from measures table depending on a condition,
#' and add them to events table
#'
#' @return A AnalysR model
#'
#' @param model An AnalysR env
#' @param condition A condition.
#' @param tag_to_create Label to write in events table.
#'
#' @export
induce_event <- function(model = analysr_env, condition, tag_to_create) {
  condition <- rlang::enexpr(condition)
  tag_to_create <- gsub(" ", "_", tag_to_create)
  # Here we admit that a condition is like: tag operator value
  # e.g. Temperature > 37.5

  if (length(condition) > 2) {
      # Method with operator
      # Here we admit that a condition is like: tag operator value
      # e.g. Temperature > 38.5

      operator <- condition[[1]]
      if (is.symbol(condition[[3]])) {
        tag_to_check <- rlang::as_string(condition[[3]])[1]
        wanted_value <- condition[[2]]
        data <- subset(model$measures, tag == tag_to_check)
        data <- data[eval(rlang::call2(operator, wanted_value, data$value)), ]

      } else {
        tag_to_check <- rlang::as_string(condition[[2]])[1]
        wanted_value <- condition[[3]]
        data <- subset(model$measures, tag == tag_to_check)
        data <- data[eval(rlang::call2(operator, data$value, wanted_value)), ]
      }

  } else {
    stop("This condition does not make sense.")
  }
  n <- nrow(data)


  result <- tibble::tibble(hash = get_hash(n), stat_unit = data$stat_unit,
                       date = data$date, tag = tag_to_create)
  model$events <- rbind(model$events, result)

  model
}
