is_before <- function(entry, duration, events) {
  # check if (entry date) =< (event date) and (entry date) >= (event date + duration)
  found <- FALSE

  for (i in rownames(events)) {
    start <- events[i,]$date - duration
    end <- events[i,]$date
    if ((entry$date <= end) && (start <= entry$date)) {
      found = TRUE
      break
    }
  }
  events[i,]$stat_unit
}

is_before_list <- function(entries, duration, events) {
  status <- c()
  for (i in rownames(entries)) {
    status <- c(status, is_before(entries[i,], duration, events))
  }
  unique(status)
}

#' before
#'
#' @export
before <- function(model, event_tag) {

  if (model$query$duration_type == "at_most"){

    res <- c()

    duration <- model$query$duration
    events <- subset(model$events, tag == event_tag)

    res <- is_before_list(model$measures, duration, events)
  }


  as.double(res)

}
