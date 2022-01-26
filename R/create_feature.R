#' create_feature
#'
#' This function create a new coll named with the description tag in the
#' stat_units table. The function will extract feature form the measure table.
#' You need to specify which tag you want, in which period you want to execute
#' the aggregation method and which col name you want in the stat_units table.
#'
#' @param model An AnalysR model
#' @param tag_to_create Label to write in description table
#' @param wanted_tag The name of the measures we want to work on
#' @param start A date marking the beginning of the studied time period.
#' @param end A date marking the end of the studied time period.
#' @param aggregation_method A function to aggregate data.
#' Default: mean_aggregate
#'
#' @export

create_feature <-
  function(model,
           tag_to_create,
           wanted_tag,
           start,
           end,
           aggregation_method = mean_aggregate){

    data <- subset(model$measures, tag == wanted_tag)
    data <- subset (data, date <= end)
    data <- subset (data, date >= start)

    to_add <- tibble::tibble(stat_unit = integer(0), new = numeric(0))

    stat <- unique(model$measures$stat_unit)
    for (s in stat){
      sample <- subset(data, stat_unit == s)
      if ((n <- nrow(data)) != 0) {
        temp <- tibble::tibble(stat_unit = c(s),
                               new = aggregation_method(sample$value))
        to_add <- rbind(to_add, temp)
      }
    }

  colnames(to_add) <- c("stat_unit", tag_to_create)
  model$stat_units <- dplyr::left_join(model$stat_units, to_add, by = "stat_unit")

  model
  }

