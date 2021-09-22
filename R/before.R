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
  found
}

is_before_list <- function(entries, duration, events) {
  status <- c()
  for (i in rownames(entries)) {
    status <- c(status, is_before(entries[i,], duration, events))
  }
  status
}

#' before
#'
#' @export
before <- function(model, event) {
  nmodel <- model
  # TODO: check if at_most and tag flag exist

  duration <- nmodel$query$at_most
  events <- subset(nmodel$events, tag == event)

  nmodel$measures <- nmodel$measures[is_before_list(nmodel$measures, duration, events),]

  # delete at_most tag from query after all
  nmodel$query <- purrr::list_modify(nmodel$query,"at_most" = NULL)

  nmodel
}
