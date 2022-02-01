#' import_stat_units_df
#'
#' Import stat_units from a dataframe.
#'
#' @return A boolean (`TRUE` if no errors)
#'
#' @param input A data frame.
#' @param stat_unit A string containing the stat_unit label.
#' @param optional_data A vector containing label to import in descriptions
#' table.
#' @param model An AnalysR env.
#' Default: `analysr_env`
#'
#' @export
import_stat_units_df <-
  function(input,
            stat_unit = "stat_unit",
            optional_data,
            model = analysr_env) {

    temp <- tibble::tibble(input)

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
