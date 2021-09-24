#' before
#'
#' @export
before <- function(model, event_tag) {

  nmodel <- model

  if (nmodel$query$duration_type == "at_most"){

    res <- c()
    for (i in length(nmodel$selection)){
      events <- subset(nmodel$events, tag == event_tag)
      events <- subset(events, stat_unit == nmodel$selection$stat_unit[i])
      events <- subset(events, date <= nmodel$query$duration +
                         nmodel$selection$date[i])
      events <- subset(events, date >= nmodel$selection$date[i])

      if (length(events) > 0){
        res <- c(res, nmodel$selection$stat_unit[i])
      }
    }
  }

  else if (nmodel$query$duration_type == "at_least"){
    res <- c()
    for (i in length(nmodel$selection)){
      events <- subset(nmodel$events, tag == event_tag)
      events <- subset(events, stat_unit == nmodel$selection$stat_unit[i])
      events <- subset(events, date >= nmodel$query$duration +
                         nmodel$selection$date[i])

      if (length(events) > 0){
        res <- c(res, nmodel$selection$stat_unit[i])
      }
    }
  }

  as.double(res)

}
