#' import_events_df
#'
#' Import events from a data frame.
#'
#' @return A boolean (`TRUE` if no errors)
#'
#' @param input A data frame.
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
import_events_df <-
  function(input,
            stat_unit = "stat_unit",
            date = "date",
            tag = "tag",
            optional_data,
            model = analysr_env) {

    result <- tibble::tibble(input)
    n <- nrow(result) # get row number only one time
    hash <- get_hash(n)

    # using missing https://bit.ly/2QJJyb6
    if (!missing(optional_data)) {
      fill_descriptions(hash, optional_data, result, n, model)
    }

    result <- result[c(stat_unit, date, tag)]
    # we could use dplyr to extract colums https://bit.ly/32lGkNR
    colnames(result) <- c("stat_unit", "date", "tag")

    add_stat_units(result$stat_unit, model)

    result <- dplyr::bind_cols(
      hash = hash,
      result
    )

    model$events <- rbind(model$events, result)

    TRUE
  }
