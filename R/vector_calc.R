

#' vector_calc
#'
#' This function allow you to calcul data from a simple selection to add a
#' description. The `vector_calc` name can be missleading.
#'
#' Setting a value to `start`, `end`, and `wanted_tag` would make the
#' `vector_calc` function on measure table, almost like `create_feature`.
#'
#' If you do not provide a start/end, `vector_calc` would try to work on the
#' selection table.
#'
#' The result is always in the description table.
#' @param model An AnalysR model
#' @param tag_to_create Label to write in description table
#' @param func_to_apply A function to apply to the vector selected.
#' @param wanted_tag The name of the measures we want to work on.
#' (not mandatory)
#' @param start A date marking the beginning of the studied time period
#' (not mandatory)
#' @param end A date marking the end of the studied time period.
#' (not mandatory)
#'
#'
#' @export
vector_calc <- function(model, tag_to_create, func_to_apply, wanted_tag, start,
                        end) {
    if (!missing(start) & !missing(wanted_tag) & !missing(end)) {

        data <- subset(model$measures, tag == wanted_tag)
        data <- subset(data, date <= end)
        data <- subset(data, date >= start)
        stat <- unique(model$measures$stat_unit)

        to_add <- tibble::tibble(hash = as.integer(0),
                                    type = character(0),
                                    value = character(0))

        for (s in stat) {
            sample <- subset(data, stat_unit == s)
            if (nrow(sample) != 0) {
                temp <- tibble::tibble(
                            hash = hash_from_stat_unit(model, s),
                            type = tag_to_create,
                            value = toString(func_to_apply(sample$value))
                )
                to_add <- rbind(to_add, temp)
            }
        }
    } else {

        # tibble::tibble(hash_stat_unit, stat_unit, hash_obs,date_obs, date_obs_end))

        stat <- unique(model$selection$stat_unit)

        to_add <- tibble::tibble(hash = as.integer(0),
                                    type = character(0),
                                    value = character(0))

        for (s in stat) {
            sample <- subset(model$selection, stat_unit == s)
            if (nrow(sample) != 0) {
                measures <- subset(model$measures, hash %in% sample$hash_obs)
                if (!missing(wanted_tag)) {
                    measures <- subset(measures, tag == wanted_tag)
                }
                if (nrow(measures) != 0) {
                    temp <- tibble::tibble(
                            hash = hash_from_stat_unit(model, s),
                            type = tag_to_create,
                            value = toString(func_to_apply(measures$value))
                    )
                    # can be optimized by using selection hash_stat_unit entry
                    to_add <- rbind(to_add, temp)
                } else {
                    warning("This hash_obs does not correspond to a measure.")
                }
            }
        }
    }
    model$descriptions <- dplyr::bind_rows(model$descriptions, to_add)
    model
}
