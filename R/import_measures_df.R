#' import_measures_df
#'
#' Import measures from a data frame.
#'
#' @return A boolean (`TRUE` if no errors)
#'
#' @param input A data frame.
#' @param stat_unit A string containing the stat_unit label.
#' @param date A string containing the date label.
#' @param tag A string containing the tag label.
#' @param value A string containing the value label.
#' @param optional_data A vector containing label to import in descriptions
#' table (not required).
#' @param status A string containing the status label.
#' @param model An AnalysR env.
#' Default: `analysr_env`
#'
#' @export
import_measures_df <-
  function(input,
            stat_unit = "stat_unit",
            date = "date",
            tag = "tag",
            value = "value",
            optional_data,
            status = "status",
            model = analysr_env) {


    result <- tibble::tibble(input)
    n <- nrow(result)
    hash <- get_hash(n)

    if (!(status %in% colnames(result))) {
      result <- dplyr::bind_cols(
        result,
        status = rep(NA, n)
      )
    }

    if (!missing(optional_data)) {
      fill_descriptions(hash, optional_data, result, n, model)
    }

    result <- result[c(stat_unit, date, tag, value, status)]
    # we could use dplyr to extract colums https://bit.ly/32lGkNR
    colnames(result) <- c("stat_unit", "date", "tag", "value", "status")

    add_stat_units(result$stat_unit, model)

    result <- dplyr::bind_cols(
      hash = hash,
      result
    )

    model$measures <- rbind(model$measures, result)

    TRUE
  }
