


vector_calc <- function(model, tag_to_create, func_to_apply, start, end) {

    if (!missing(start) & !missing(end)) {
        data <- subset(model$measures, tag == wanted_tag)
        data <- subset(data, date <= end)
        data <- subset(data, date >= start)
        stat <- unique(model$measures$stat_unit)

        to_add <- tibble::tibble(hash = as.integer(0),
                                    type = character(0),
                                    value = character(0))

        for (s in stat) {
            sample <- subset(data, stat_unit == s)
            if ((n <- nrow(data)) != 0) {
                temp <- tibble::tibble(hash = hash_from_stat_unit(s),
                                    type = tag_to_create,
                                    value = func_to_apply(sample$value))
                to_add <- rbind(to_add, temp)
            }
        }

        model$descriptions <- dplyr::bind_rows(model$descriptions, to_add)
    } else {
        # tibble::tibble(hash_stat_unit, stat_unit, hash_obs,date_obs, date_obs_end))
        stat <- unique(model$selection$stat_unit)

        to_add <- tibble::tibble(hash = as.integer(0),
                                    type = character(0),
                                    value = character(0))

        for (s in stat) {
            sample <- subset(model$selection, stat_unit == s)
            if (nrow(data) != 0) {
                measures <- subset(model$measures, hash %in% sample$hash_obs)
                if (nrow(measures) != 0) {
                    temp <- tibble::tibble(hash = hash_from_stat_unit(s),
                                        type = tag_to_create,
                                        value = func_to_apply(measures$value))
                    # can be optimized by using selection hash_stat_unit entry
                    to_add <- rbind(to_add, temp)
                } else {
                    warning("This hash_obs does not correspond to a measure.")
                }
            }
        }
    }
    model$descriptions <- dplyr::bind_rows(model$descriptions, to_add)
}
