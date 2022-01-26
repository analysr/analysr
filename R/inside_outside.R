
#' inside
#'
#' The inside function is used to return the set of events or measurements that
#' verify a certain condition observed during a certain period.
#'
#' @param model An AnalysR model.
#' @param period_wanted A period from the period table.
#'
#' @export
inside <- function (model, period_wanted) {
    # TODO: check if period is a string or an expression

    # select period corresponding
    periods_selected <- subset(model$periods, tag == period_wanted)

    to_keep <- c()
    for (i in rownames(model$selection)) {
        # we select the period corresponding to a unique stat_unit
        periods_selected_unit <- subset(periods_selected,
                                    stat_unit == model$selection[i,]$stat_unit)
        if (nrow(periods_selected_unit) == 0) {
            to_keep <- c(to_keep, FALSE)
        } else {
            date <- as.numeric(model$selection[i,]$date_obs)
            found <- FALSE
            for (j in rownames(periods_selected_unit)) {
                begin <- as.numeric(periods_selected_unit$begin)
                end <- as.numeric(periods_selected_unit$end)
                if (date >= begin && date <= end) {
                    found <- TRUE
                }
            }
            if (found) {
                to_keep <- c(to_keep, TRUE)
            } else {
                to_keep <- c(to_keep, FALSE)
            }
        }
    }

    model$selection <- model$selection[to_keep, ]

    model
}

#' during
#'
#' @rdname inside
#' @param model An AnalysR model.
#' @param period_wanted A period from the period table.
#'
#' @export
during <- inside

#' outside
#'
#' The outside function is used to return the set of events or measurements
#' that verify a certain condition observed outside a certain period.
#'
#' @param model An AnalysR model.
#' @param period_wanted A period from the period table.
#'
#' @export
outside <- function (model, period_wanted) {
    # TODO: check if period is a string or an expression

    # select period corresponding
    periods_selected <- subset(model$periods, tag == period_wanted)

    to_keep <- c()
    for (i in rownames(model$selection)) {
        # we select the period corresponding to a unique stat_unit
        periods_selected_unit <- subset(periods_selected,
                                    stat_unit == model$selection[i,]$stat_unit)
        if (nrow(periods_selected_unit) == 0) {
            to_keep <- c(to_keep, TRUE)
        } else {
            date <- as.numeric(model$selection[i,]$date_obs)
            found <- FALSE
            for (j in rownames(periods_selected_unit)) {
                begin <- as.numeric(periods_selected_unit$begin)
                end <- as.numeric(periods_selected_unit$end)
                if (date >= begin && date <= end) {
                    found <- TRUE
                }
            }
            if (found) {
                to_keep <- c(to_keep, FALSE)
            } else {
                to_keep <- c(to_keep, TRUE)
            }
        }
    }

    model$selection <- model$selection[to_keep, ]

    model
}