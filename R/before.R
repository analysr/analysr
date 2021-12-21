is_before <- function(entry, duration, selection, type) {
  # here selection refer to the selection done by condition on before
  found <- FALSE

  temporal_hash <- -1

  if (is.na(entry$date_obs_end)){
    #means it's not a period
    date <- as.numeric(entry$date_obs)
  } else {
    #means it's a period, we take the date of the end
    date <- as.numeric(entry$date_obs_end)
  }

  if (type == "at_most") {
    for (i in rownames(selection)) {
      start <- as.numeric(selection[i,]$date_obs - duration)
      end <- as.numeric(selection[i,]$date_obs)
      # check if (entry date) =< (event date)
      #      and (entry date) >= (event date + duration)
      if ((date <= end) && (start <= date)) {
        found <- TRUE
        temporal_hash <- selection[i,]$hash_obs
        break
      }
    }
  }
  if (type == "at_least") {
    for (i in rownames(selection)) {
      max <- as.numeric(selection[i,]$date_obs - duration)
      # check if (entry date) =< (event date - duration)
      if (date <= max) {
        found <- TRUE
        temporal_hash <- selection[i,]$hash_obs
        break
      }
    }
  }

  return(list("found" = found, "temporal_hash" = temporal_hash))
}

is_before_list <- function(entries, duration, selection, type) {
  # here selection refer to the selection done by condition on before
  res <- tibble::tibble()
  splitted <- dplyr::group_split(entries, stat_unit)
  for (tbl in splitted) {
    selection_by_stat_units <- dplyr::filter(selection, stat_unit == tbl$stat_unit[1])
    for (i in rownames(tbl)) {
      tmp <- is_before(tbl[i,], duration, selection_by_stat_units, type)

      if (tmp$found) {
        new_entry <- dplyr::bind_cols(tbl[i,], temporal_hash = tmp$temporal_hash)
        res <- dplyr::bind_rows(res, new_entry)
      }
    }
  }
  res
}

#' before
#'
#' @param model An AnalysR model
#' @param condition A condition
#'
#' @export
before <- function(model, condition) {

  if (!("duration_type" %in% names(model$query))){
    model$query$duration_type <- "at_least"
    model$query$duration <- lubridate::duration(0)
  }
  duration <- model$query$duration
  condition <- rlang::enexpr(condition)
  before_selection <- prepare_query(model, condition)

  tictoc::tic("is_before_list")
  res <- is_before_list(model$selection, duration,
                        before_selection, model$query$duration_type)
  tictoc::toc()

  model$selection <- res
  model

}
