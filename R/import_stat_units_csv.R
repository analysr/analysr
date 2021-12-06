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


    temp <- quiet_read_csv(file = csv_path, delim = delim)$result



    stat_units <- temp[c(stat_unit)]
    # we could use dplyr to extract colums https://bit.ly/32lGkNR
    colnames(stat_units) <- c("stat_unit")


    to_add <- unique(stat_units$stat_unit
                [!(stat_units$stat_unit %in% analysr_env$stat_units$stat_unit)])
    n <- length(to_add)
    if (n != 0) {
        hashs <- get_hash(n)
        result <- tibble::tibble(hashs, to_add)
        # should define name https://bit.ly/2QvwrdM
        colnames(result) <- c("hash", "stat_unit")


        if (!missing(optional_data)) {
          fill_descriptions(hashs, optional_data, temp, n)
        }
        analysr_env$stat_units <- rbind(analysr_env$stat_units, result)
    }



    TRUE
}
