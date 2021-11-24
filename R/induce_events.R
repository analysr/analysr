#' induce_event
#'
#' Create event from measures table depending on a condition,
#' and add them to events table
#'
#' @return The part of event tablethat have been created.
#'
#' @param condition A condition.
#' @param event An event name.
#'
#' @export
induce_events <- function(condition, event) {
  condition <- rlang::enexpr(condition)
  # Here we admit that a condition is like: tag operator value
  # e.g. Temperature > 37.5

  tag_to_check <- rlang::as_string(condition[[2]])[1]
  operator <- condition[[1]]
  wanted_value <- condition[[3]]

  data <- subset(analysr_env$measures, tag == tag_to_check)
  data <- data[eval(rlang::call2(operator, data$value, wanted_value)),]

  n <- nrow(data)

  result <- tibble::tibble(hash = get_hash(n), stat_unit=data$stat_unit,
                       date=data$date, tag=event)

  analysr_env$events <- rbind(analysr_env$events, result)
}
