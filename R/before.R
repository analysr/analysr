is_before <- function(entry, duration, events) {
  # check if (entry date) =< (event date) and (entry date) >= (event date + duration)
  found <- FALSE
  print(entry)
  for (i in rownames(events)) {
    end <- events[i,]$date - duration
    if (entry$date %within% list(events[i,]$date, end)) {
      break
    }
  }
  found
}

is_before_list <- function(entries, duration, events) {
  status <- c()
  for (i in rownames(entries)) {
    status <- c(status, is_before(entries[i,], duration, events))
  }
  status
}

before <- function(model, event) {
  nmodel <- model
  # TODO: check if at_most and tag flag exist

  duration <- nmodel$query$at_most
  events <- subset(nmodel$events, tag == event)

  nmodel$measures <- nmodel$measures[is_before_list(nmodel$measures, duration, events)]

  nmodel
}
