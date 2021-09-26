#' before
#'
#' @export
before <- function(model, event_tag) {

  if (model$query$duration_type == "at_most"){

    res <- c()
    for (i in length(model$selection)){
      events <- subset(model$events, tag == event_tag)
      events <- subset(events, stat_unit == model$selection$stat_unit[i])
      events <- subset(events, date <= model$query$duration +
                         model$selection$date[i])
      events <- subset(events, date >= model$selection$date[i])

      if (length(events) > 0){
        res <- c(res, model$selection$stat_unit[i])
      }
    }
  }

  else if (model$query$duration_type == "at_least"){
    res <- c()
    for (i in length(model$selection)){
      events <- subset(model$events, tag == event_tag)
      events <- subset(events, stat_unit == model$selection$stat_unit[i])
      events <- subset(events, date >= model$query$duration +
                         model$selection$date[i])

      if (length(events) > 0){
        res <- c(res, model$selection$stat_unit[i])
      }
    }
  }

  as.double(res)

}
