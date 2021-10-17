is_before <- function(entry, duration, events, type) {

  found <- FALSE
  events <- subset(events, stat_unit == entry$stat_unit)


  if (type == "at_most") {
    for (i in rownames(events)) {
      date <- as.numeric(entry$date)
      start <- as.numeric(events[i,]$date - duration)
      end <- as.numeric(events[i,]$date)
      # check if (entry date) =< (event date)
      #      and (entry date) >= (event date + duration)
      if ((date <= end) && (start <= date)) {
        found <- TRUE
        break
      }
    }
  }
  if (type == "at_least") {
    for (i in rownames(events)) {
      date <- as.numeric(entry$date)
      max <- as.numeric(events[i,]$date - duration)
      # check if (entry date) =< (event date - duration)
      if (date <= max) {
        found <- TRUE
        break
      }
    }
  }

  if (found == TRUE) {
    entry$stat_unit
  } else {
    c()
  }

}

is_before_list <- function(entries, duration, events, type) {
  stat_units <- c()
  for (i in rownames(entries)) {
    stat_unit <- is_before(entries[i,], duration, events, type)

    stat_units <- c(stat_units, stat_unit)
  }
  unique(stat_units)
}

#' before
#'
#' @export
before <- function(model, event_tag) {


  res <- c()

  duration <- model$query$duration
  events <- subset(model$events, tag == event_tag)

  res <- is_before_list(model$selection, duration,
                        events, model$query$duration_type)



  as.double(res)

}
