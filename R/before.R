#' is_before <- function(entry, duration, events) {
#'   # check if (entry date) =< (event date) and (entry date) >= (event date + duration)
#'   found <- FALSE
#'
#'   for (i in rownames(events)) {
#'     start <- events[i,]$date - duration
#'     end <- events[i,]$date
#'     if ((entry$date <= end) && (start <= entry$date)) {
#'       found = TRUE
#'       break
#'     }
#'   }
#'   found
#' }
#'
#' is_before_list <- function(entries, duration, events) {
#'   status <- c()
#'   for (i in rownames(entries)) {
#'     status <- c(status, is_before(entries[i,], duration, events))
#'   }
#'   status
#' }
#'
#' #' before
#' #'
#' #' @export
#' before <- function(model, event_tag) {
#'   nmodel <- model
#'   # TODO: check if at_most and tag flag exist
#'
#'   duration <- nmodel$query$at_most
#'   events <- subset(nmodel$events, tag == event_tag)
#'
#'   nmodel$measures <- nmodel$measures[is_before_list(nmodel$measures, duration,
#'                                                     events),]
#'
#'   # delete at_most tag from query after all
#'   nmodel$query <- purrr::list_modify(nmodel$query,"at_most" = NULL)
#'
#'   nmodel
#' }






#' before
#'
#' @export
before <- function(model, event_tag) {

  nmodel <- model

  if (nmodel$query$duration_type == "at_most"){

    res <- c()
    for (i in length(nmodel$query$selection)){
      events <- subset(nmodel$events, tag == event_tag)
      events <- subset(events, stat_unit == nmodel$query$selection$stat_unit[i])
      events <- subset(events, date <= nmodel$query$duration +
                         nmodel$query$selection$date[i])
      events <- subset(events, date >= nmodel$query$selection$date[i])

      if (length(events) > 0){
        res <- c(res, nmodel$query$selection$stat_unit[i])
      }
    }
  }

  else if (nmodel$query$duration_type == "at_least"){
    res <- c()
    for (i in length(nmodel$query$selection)){
      events <- subset(nmodel$events, tag == event_tag)
      events <- subset(events, stat_unit == nmodel$query$selection$stat_unit[i])
      events <- subset(events, date >= nmodel$query$duration +
                         nmodel$query$selection$date[i])

      if (length(events) > 0){
        res <- c(res, nmodel$query$selection$stat_unit[i])
      }
    }
  }

  as.double(res)

}
