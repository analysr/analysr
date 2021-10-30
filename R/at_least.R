#' at_least
#'
#' @param model An AnalysR model
#' @param e A duration expression like: 1*days
#'
#' @export
at_least <- function (model, e) {

  model$query$duration_type <- "at_least"

  e <- rlang::enexpr(e)

  n <- e[[2]]
  unit <- rlang::as_string(e[[3]])[1]
  # TODO: create an aux fnct that can understand 1*days+2*hours

  model$query$duration <- lubridate::duration(n, unit)

  model
}
