#' import_events_csv
#'
#' Import events from a CSV file
#'
#' @return The events data frame resulted from the merge of imported data 
#' and already imported data
#'
#' @param csv_path A path to the csv file.
#' @param stat_unit A string containing the stat_unit label.
#' @param date A string containing the date label.
#' @param tag A string containing the tag label.
#' 
#' @examples
#' import_events_csv(csv_path, stat_unit, date, tag)
import_events_csv <-
  function (csv_path,
            stat_unit = "stat_unit",
            date = "date",
            tag = "tag") {
    quiet_read_csv <- purrr::quietly(readr::read_csv)

    result <- quiet_read_csv(file = csv_path)$result
    result <- as.data.frame(result)

    result <- result[c(stat_unit, date, tag)]
    # https://stackoverflow.com/questions/10085806/extracting-specific-columns-from-a-data-frame
    # on pourrait utiliser dplyr pour extraire des colonnes
    colnames(result) <- c("stat_unit", "date", "tag")

    result <- cbind(
      hash = get_hash(nrow(result)),
      result
    )

    analysr_env$events <- rbind(analysr_env$events, result)
    result
  }
