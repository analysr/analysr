isDate <- function(x) {
  inherits(x, c("Date", "POSIXt"))
}

#' from
#'
#' @param model A Analysr model instance.
#' @param date A date object or a string to be converted as a date.
#' @param date_format_func A function to format date with (not required).
#' Default: `lubridate::parse_date_time(x, date_format_reg)`
#' @param date_format_reg A expression to format date with (not required).
#' Default: `"ymd-HMS"`
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


#' to
#'
#' @param model A Analysr model instance.
#' @param date A date object or a string to be converted as a date.
#' @param date_format_func A function to format date with (not required).
#' Default: `lubridate::parse_date_time(x, date_format_reg)`
#' @param date_format_reg A expression to format date with (not required).
#' Default: `"ymd-HMS"`
#'
#' @export
to <- function(model, date, date_format_func =
                  (function(x) lubridate::parse_date_time(x, date_format_reg)),
                  date_format_reg = "ymd-HMS") {

  if (isDate(date)) {
    model$query$to_date <- date
  } else {
    model$query$to_date <- date_format_func(date)
  }

  to_keep = c()
  for (i in rownames(model$selection)) {
    date = as.numeric(model$selection[i,]$date)
    if (date >= as.numeric(model$query$from_date) && date <= as.numeric(model$query$to_date)) {
      to_keep <- c(to_keep, TRUE)
    } else {
      to_keep <- c(to_keep, FALSE)
    }
  }

  model$selection <- model$selection[to_keep, ]

  model
}
