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
  function(csv_path,
            stat_unit = "stat_unit",
            begin = "begin",
            end = "end",
            desc = "desc",
            optional_data) {
    quiet_read_csv <- purrr::quietly(readr::read_csv)

    result <- quiet_read_csv(file = csv_path)$result
    result <- as.data.frame(result)

    n <- nrow(result)
    hash <- get_hash(n)

    if (!missing(optional_data)) {
      fill_descriptions(hash, optional_data, result, n)
    }

    result <- result[c(stat_unit, begin, end, desc)]
    # we could use dplyr to extract colums https://bit.ly/32lGkNR
    colnames(result) <- c("stat_unit", "begin", "end", "desc")

    add_stat_units(result$stat_unit)

    result <- cbind(
      hash,
      result
    )

    analysr_env$periods <- rbind(analysr_env$periods, result)
    result
  }
