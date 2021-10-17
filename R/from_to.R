#' from
#'
#' @export
from <- function (model, date) {

  model$query$from_date <- date

  model
}
