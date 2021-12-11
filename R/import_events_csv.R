#' import_events_csv
#'
#' Import events from a CSV file
#'
#' @return A boolean (`TRUE` if no errors)
#'
#' @param csv_path A path to the csv file.
#' @param stat_unit A string containing the stat_unit label.
#' @param date A string containing the date label.
#' @param tag A string containing the tag label.
#' @param optional_data A vector containing label to import in descriptions
#' table.
#' @param date_format_func A function to format date with (not required).
#' Default: `lubridate::parse_date_time(x, date_format_reg)`
#' If you want to use milliseconds [look at this](https://bit.ly/33JGr6s).
#' @param date_format_reg A expression to format date with (not required).
#' Default: `"ymd-HMS"`
#' For more details see [this documentation](https://bit.ly/3bp3FD0).
#' @param force_date_format Boolean to force date format func (not required).
#' Default: `FALSE`
#' @param delim The separator to read csv (not required).
#' Default: `,`
#' @param model An AnalysR env.
#' Default: `analysr_env`
#'
#' @export
import_events_csv <-
  function(csv_path,
            stat_unit = "stat_unit",
            date = "date",
            tag = "tag",
            optional_data,
            date_format_func =
                  (function(x) lubridate::parse_date_time(x, date_format_reg)),
            date_format_reg = "ymd-HMS",
            force_date_format = FALSE,
            delim = ",",
            model = analysr_env) {
    quiet_read_csv <- purrr::quietly(readr::read_delim)

    if (force_date_format) {
      result_csv <- quiet_read_csv(file = csv_path,
                               col_types = readr::cols(date = "c"),
                               delim = delim)$result
    } else {
      result_csv <- quiet_read_csv(file = csv_path,
                               delim = delim)$result
    }



    n <- nrow(result_csv) # get row number only one time
    hash <- get_hash(n)

    # using missing https://bit.ly/2QJJyb6
    if (!missing(optional_data)) {
      fill_descriptions(hash, optional_data, result_csv, n, model)
    }

    result <- result_csv[c(stat_unit, date, tag)]
    # we could use dplyr to extract colums https://bit.ly/32lGkNR
    colnames(result) <- c("stat_unit", "date", "tag")

    add_stat_units(result$stat_unit, model)

    if (!isDate(result$date) || force_date_format) {
      result$date <- date_format_func(result$date)
    }

    result <- dplyr::bind_cols(
      hash = hash,
      result
    )

    model$events <- rbind(model$events, result)

    TRUE
  }
