#' import_stat_units_csv
#'
#' Import stat_units from a CSV file.
#'
#' @return A boolean (`TRUE` if no errors)
#'
#' @param csv_path A path to the csv file.
#' @param stat_unit A string containing the stat_unit label.
#' @param optional_data A vector containing label to import in descriptions
#' table.
#' @param delim The separator to read csv (not required).
#' Default: `,`
#' @param model An AnalysR env.
#' Default: `analysr_env`
#'
#' @export
import_stat_units_csv <-
  function(csv_path,
            stat_unit = "stat_unit",
            optional_data,
            delim = ",",
            model = analysr_env) {
    quiet_read_csv <- purrr::quietly(readr::read_delim)


    temp <- quiet_read_csv(file = csv_path, delim = delim)$result



    stat_units <- temp[c(stat_unit)]
    # we could use dplyr to extract colums https://bit.ly/32lGkNR
    colnames(stat_units) <- c("stat_unit")


    add_stat_units(stat_units$stat_unit, model)

    hashs <- hash_from_stat_unit(model, stat_units$stat_unit)
    if (!missing(optional_data)) {
      fill_descriptions(hashs, optional_data, temp, n, model)
    }

    # here I assume that I can have a description multiple times
    # for example
    # 1,FIRST,Jacinto644
    # 1,BIRTHDATE,2017-07-27
    # 1,FIRST,Jacinto644
    # because a line has been already imported

    TRUE
}
