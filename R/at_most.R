#' at_most
#'
#' @param model An AnalysR model
#' @param e A duration expression like: 1*days
#'
#' @export
at_most <- function(model, e) {
  model$query$duration_type <- "at_most"
  e <- rlang::enexprs(e)
  e <- toString(e)
  e <- tolower(e)

  e <- stringr::str_replace_all(e, "years", "31557600")
  e <- stringr::str_replace_all(e, "year", "31557600")

  e <- stringr::str_replace_all(e, "months", "2678400")
  e <- stringr::str_replace_all(e, "month", "2678400")

  e <- stringr::str_replace_all(e, "weeks", "604800")
  e <- stringr::str_replace_all(e, "week", "604800")

  e <- stringr::str_replace_all(e, "days", "86400")
  e <- stringr::str_replace_all(e, "day", "86400")

  e <-stringr::str_replace_all(e,  "hours", "3600")
  e <-stringr::str_replace_all(e,  "hour", "3600")

  e <-stringr::str_replace_all(e,  "minutes", "60")
  e <-stringr::str_replace_all(e,  "minute", "60")

  e <-stringr::str_replace_all(e,  "seconds", "1")
  e <-stringr::str_replace_all(e,  "second", "1")

  durationSeconds <- eval(str2expression(e))
  model$query$duration <- lubridate::duration(durationSeconds, "seconds")

  model
}
