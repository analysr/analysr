isDate <- function(x) {
  inherits(x, c("Date", "POSIXt"))
}

#' from
#'
#' @export
from <- function(model, date, date_format_func =
                  (function(x) lubridate::parse_date_time(x, date_format_reg)),
                  date_format_reg = "ymd-HMS") {

  if (isDate(date)) {
    model$query$from_date <- date
  } else {
    model$query$from_date <- date_format_func(date)
  }

  model
}
