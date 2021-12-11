
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

  model$query$duration <- get_duration_from_str(e)

  model
}

#' at_least
#'
#' @param model An AnalysR model
#' @param e A duration expression like: 1*days
#'
#' @export
at_least <- function (model, e) {

  model$query$duration_type <- "at_least"

  e <- rlang::enexprs(e)
  e <- toString(e)

  model$query$duration <- get_duration_from_str(e)

  model
}
