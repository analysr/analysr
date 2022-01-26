#' induce_period
#'
#' Create period from measures table depending on a condition (on measures or
#' events), and add them to periods table.
#'
#' @return A AnalysR model
#'
#' @param model An AnalysR env.
#' Default: `analysr_env`
#' @param condition A condition (to be defined).
#' @param tag_to_create Label to write in periods table.
#' @param duration A duration.
#'
#' @export
induce_period <- function(model = analysr_env, condition, tag_to_create,
                          duration) {
  if (model$space_to_underscore) {
    tag_to_create <- gsub(" ", "_", tag_to_create)
  }

  condition <- rlang::enexpr(condition)
  duration <- rlang::enexprs(duration)
  duration <- toString(duration)

  duration <- get_duration_from_str(duration)

  if (length(condition) > 2) {
      # Method with operator
      # Here we admit that a condition is like: tag operator value
      # e.g. Temperature > 38.5

      operator <- condition[[1]]
      if (is.symbol(condition[[3]])) {
        tag_to_check <- rlang::as_string(condition[[3]])[1]
        wanted_value <- condition[[2]]
        data <- subset(model$measures, tag == tag_to_check)
        data <- data[eval(rlang::call2(operator, wanted_value, data$value)),]

      } else {
        tag_to_check <- rlang::as_string(condition[[2]])[1]
        wanted_value <- condition[[3]]
        data <- subset(model$measures, tag == tag_to_check)
        data <- data[eval(rlang::call2(operator, data$value, wanted_value)), ]
      }

  } else {
    # Method without operator

    tag_to_check <- rlang::as_string(condition)

    data <- subset(model$events, tag == tag_to_check)


  }
  n <- nrow(data)

  result <- tibble::tibble(hash = get_hash(n), stat_unit = data$stat_unit,
                       begin = data$date, end = data$date + duration,
                       tag = rep(tag_to_create, n))

  model$periods <- rbind(model$periods, result)

  model
}
