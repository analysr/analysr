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
#' table.
#' @param status A string containing the status label.
#' 
#' @examples
#' import_measures_csv(csv_path, stat_unit, date, tag, value)
import_measures_csv <-
  function(csv_path,
            stat_unit = "stat_unit",
            date = "date",
            tag = "tag",
            value = "value",
            optional_data,
            status = "status") {
    quiet_read_csv <- purrr::quietly(readr::read_csv)

    result <- quiet_read_csv(file = csv_path)$result
    result <- as.data.frame(result)

    n <- nrow(result)
    hash <- get_hash(n)

    if (!("status" %in% colnames(result))) {
      result <- cbind(
        result,
        status = rep("", n)
      )
    } 

    if (!missing(optional_data)) {
      fill_descriptions(hash,optional_data, result,n)
    }

    result <- result[c(stat_unit, date, tag, value, status)]
    # we could use dplyr to extract colums https://bit.ly/32lGkNR
    colnames(result) <- c("stat_unit", "date", "tag", "value", "status")

    add_stat_units(result$stat_unit)

    result$date <- lubridate::ymd_hms(result$date)

    result <- cbind(
      hash,
      result
    )

    analysr_env$measures <- rbind(analysr_env$measures, result)
    result
  }
