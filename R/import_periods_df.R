#' import_periods_df
#'
#' Import periods from a data frame.
#'
#' @return A boolean (`TRUE` if no errors)
#'
#' @param input A data frame.
#' @param stat_unit A string containing the stat_unit label.
#' @param begin A string containing the begin date label.
#' @param end A string containing the end date label.
#' @param tag A string containing the tag label.
#' @param optional_data A vector containing label to import in descriptions
#' table.
#' @param model An AnalysR env.
#' Default: `analysr_env`
#'
#' @export
import_periods_df <-
  function(input,
            stat_unit = "stat_unit",
            begin = "begin",
            end = "end",
            tag = "tag",
            optional_data,
            model = analysr_env) {

    result <- tibble::tibble(input)

    n <- nrow(result)
    hash <- get_hash(n)

    if (!missing(optional_data)) {
      fill_descriptions(hash, optional_data, result, n, model)
    }

    result <- result[c(stat_unit, begin, end, tag)]
    # we could use dplyr to extract colums https://bit.ly/32lGkNR
    colnames(result) <- c("stat_unit", "begin", "end", "tag")

    add_stat_units(result$stat_unit, model)

    result <- dplyr::bind_cols(
      hash = hash,
      result
    )

    model$periods <- rbind(model$periods, result)

    TRUE
  }