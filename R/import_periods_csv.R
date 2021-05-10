#' import_periods_csv
#'
#' Import periods from a CSV file
#'
#' @return The periods data frame resulted from the merge of imported data 
#' and already imported data
#'
#' @param csv_path A path to the csv file.
#' @param stat_unit A string containing the stat_unit label.
#' @param begin A string containing the begin date label.
#' @param end A string containing the end date label.
#' @param desc A string containing the desc label.
#'
#' @examples
#' import_periods_csv(csv_path, stat_unit, begin, end, desc)
import_periods_csv <-
  function (csv_path,
            stat_unit = "stat_unit",
            begin = "begin",
            end = "end",
            desc = "desc") {
    quiet_read_csv <- purrr::quietly(readr::read_csv)

    result <- quiet_read_csv(file = csv_path)$result
    result <- as.data.frame(result)

    result <- result[c(stat_unit, begin, end, desc)]
    # https://stackoverflow.com/questions/10085806/extracting-specific-columns-from-a-data-frame
    # on pourrait utiliser dplyr pour extraire des colonnes
    colnames(result) <- c("stat_unit", "begin", "end", "desc")
    analysr_env$periods <- rbind(analysr_env$periods, result)
    result
  }
