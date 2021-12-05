is_after <- function(entry, duration, selection, type) {
  # here selection refer to the selection done by condition on after
  found <- FALSE
  selection <- subset(selection, stat_unit == entry$stat_unit)
  temporal_hash <- -1

  if (type == "at_most") {
    for (i in rownames(selection)) {
      date <- as.numeric(entry$date_obs)
      if (is.na(entry$date_obs_end)){#means it's not a period
        start <- as.numeric(selection[i,]$date_obs)
        end <- as.numeric(selection[i,]$date_obs + duration)}
      else {#means it's a period, we take the date of the end
        start <- as.numeric(selection[i,]$date_obs_end)
        end <- as.numeric(selection[i,]$date_obs_end + duration)}
      # check if (selection date) <= (entry date) =< (selection date + duration)
      if ((start <= date) && (date <= end)) {
        found <- TRUE
        temporal_hash <- selection[i,]$hash_obs
        break
      }
    }
  }



      else {#means it's a period, we take the date of the end
        date <- as.numeric(entry$date_obs_end)}


  if (type == "at_least") {
    for (i in rownames(selection)) {
      date <- as.numeric(entry$date_obs)
      if (is.na(entry$date_obs_end)){#means it's not a period
        min <- as.numeric(selection[i,]$date_obs + duration)}
      else {#means it's a period, we take the date of the end
        min <- as.numeric(selection[i,]$date_obs_end + duration)}
      # check if (entry date) >= (event date + duration)
      if (date >= min) {
        found <- TRUE
        temporal_hash <- selection[i,]$hash_obs
        break
      }
    }
  }

  return(list("found" = found, "temporal_hash" = temporal_hash))
}

is_after_list <- function(entries, duration, selection, type) {
  # here selection refer to the selection done by condition on after
  res <- tibble::tibble()
  for (i in rownames(entries)) {
    tmp <- is_after(entries[i,], duration, selection, type)

    if (tmp$found) {
      new_entry <- dplyr::bind_cols(entries[i,], temporal_hash = tmp$temporal_hash)
      res <- dplyr::bind_rows(res, new_entry)
    }
  }
  res
}

#' after
#'
#' @param model An AnalysR model
#' @param condition A condition
#'
#' @export
after <- function(model, condition) {

  if (!("duration_type" %in% names(model$query))){
    model$query$duration_type <- "at_least"
    model$query$duration <- lubridate::duration(0)
  }
  duration <- model$query$duration
  condition <- rlang::enexpr(condition)
  after_selection <- prepare_query(model, condition)

  # TODO: improve periods handling (here only start date are take into account)

  res <- is_after_list(model$selection, duration,
                        after_selection, model$query$duration_type)


  model$selection <- res
  model

}
