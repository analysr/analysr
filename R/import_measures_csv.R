#' import_measures_csv
#'
#' Import measures from a CSV file
#'
#' @return The measures data frame resulted from the merge of imported data
#' and already imported data
#'
#' @param csv_path A path to the csv file.
#' @param stat_unit A string containing the stat_unit label.
#' @param date A string containing the date label.
#' @param tag A string containing the tag label.
#' @param value A string containing the value label.
#' @param optional_data A vector containing label to import in descriptions
#' table (not required).
#' @param status A string containing the status label.
#' @param date_format_func A function to format date with (not required).
#' Default: `lubridate::parse_date_time(x, date_format_reg)`
#' If you want to use milliseconds [look at this](https://bit.ly/33JGr6s).
#' @param date_format_reg A expression to format date with (not required).
#' Default: `"ymd-HMS"`
#' For more details see [this documentation](https://bit.ly/3bp3FD0).
#' @export
import_measures_csv <-
  function(csv_path,
            stat_unit = "stat_unit",
            date = "date",
            tag = "tag",
            value = "value",
            optional_data,
            status = "status",
            date_format_func =
                  (function(x) lubridate::parse_date_time(x, date_format_reg)),
            date_format_reg = "ymd-HMS") {

    quiet_read_csv <- purrr::quietly(readr::read_csv)

    result <- quiet_read_csv(file = csv_path,
                             col_types = readr::cols(date = "c"))$result
    result <- as.data.frame(result)

    n <- nrow(result)
    hash <- get_hash(n)

    if (!(status %in% colnames(result))) {
      result <- cbind(
        result,
        status = rep(NA, n)
      )
    }

    if (!missing(optional_data)) {
      fill_descriptions(hash, optional_data, result,n)
    }

    result <- result[c(stat_unit, date, tag, value, status)]
    # we could use dplyr to extract colums https://bit.ly/32lGkNR
    colnames(result) <- c("stat_unit", "date", "tag", "value", "status")

    result$date <- date_format_func(result$date)

    add_stat_units(result$stat_unit)

    result <- cbind(
      hash,
      result
    )

    analysr_env$measures <- rbind(analysr_env$measures, result)
    result
  }
