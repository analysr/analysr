is_before <- function(entry, duration, selection, type) {

  found <- FALSE
  selection <- subset(selection, stat_unit == entry$stat_unit)


  if (type == "at_most") {
    for (i in rownames(selection)) {
      date <- as.numeric(entry$date)
      start <- as.numeric(selection[i,]$date_obs - duration)
      end <- as.numeric(selection[i,]$date_obs)
      # check if (entry date) =< (event date)
      #      and (entry date) >= (event date + duration)
      if ((date <= end) && (start <= date)) {
        found <- TRUE
        break
      }
    }
  }
  if (type == "at_least") {
    for (i in rownames(selection)) {
      date <- as.numeric(entry$date)
      max <- as.numeric(selection[i,]$date_obs - duration)
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

is_before_list <- function(entries, duration, selection, type) {
  stat_units <- c()
  for (i in rownames(entries)) {
    stat_unit <- is_before(entries[i,], duration, selection, type)

    stat_units <- c(stat_units, stat_unit)
  }
  unique(stat_units)
}

#' before
#'
#' @param model An AnalysR model
#' @param condition A condition
#'
#' @export
before <- function(model, condition) {


  res <- c()

  duration <- model$query$duration
  condition <- rlang::enexpr(condition)
  before_selection <- prepare_query(model, condition)

  # TODO: improve periods handeling (here only start date are take into account)
  # TODO2: what to do with NA values when condition is on descriptions table ?

  res <- is_before_list(model$selection, duration,
                        before_selection, model$query$duration_type)



  as.double(res)

}
