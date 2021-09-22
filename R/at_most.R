#' at_most
#'
#' @export
at_most <- function (model, e) {
  nmodel <- model
  e <- rlang::enexpr(e)

  n <- e[[2]]
  unit <- rlang::as_string(e[[3]])[1]
  # TODO: create an aux fnct that can understand 1*days+2*hours

  nmodel$query$at_most <- lubridate::duration(n, unit)

  nmodel
}
