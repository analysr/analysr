#' import_stat_units_csv
#'
#' Import stat_units from a CSV file
#'
#' @return TRUE if success
#'
#' @param csv_path A path to the csv file.
#' @param stat_unit A string containing the stat_unit label.
#' @param optional_data A vector containing label to import in descriptions
#' table.
#' @param delim The separator to read csv (not required).
#' Default: `,`
#'
#'
#' @export
import_stat_units_csv <-
  function(csv_path,
            stat_unit = "stat_unit",
            optional_data,
            delim = ",") {
    quiet_read_csv <- purrr::quietly(readr::read_delim)


    result <- quiet_read_csv(file = csv_path, delim = delim)$result

    n <- nrow(result)
    hash <- get_hash(n)

    if (!missing(optional_data)) {
      fill_descriptions(hash, optional_data, result, n)
    }

    result <- result[c(stat_unit)]
    # we could use dplyr to extract colums https://bit.ly/32lGkNR
    colnames(result) <- c("stat_unit")



    add_stat_units(result$stat_unit)



    TRUE
}
