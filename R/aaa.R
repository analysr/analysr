#' analysr_env
#'
#' @export
analysr_env <- new.env(parent = emptyenv())

attr(analysr_env, "name") <- "AnalysR env"

#' show_env
#' @param env An environment variable (not mandatory)
#' @export
show_env <- function(env) {
  if (missing(env)) {
    env <- analysr_env
  }
  print(paste("Showing env: ", environmentName(env)))
  print(sapply(ls(env), function(x) get(x, envir = env)))
}


#' Setup new envirenment
#'
#' @examples
#' setup_new_env()
#' @export
setup_new_env <- function() {
    # create query
    analysr_env$query <- list()

    # create data frame for measures
    analysr_env$measures <- tibble::tibble(hash = integer(0),
                                    stat_unit = character(0),
                                    date = as.POSIXct(NA),
                                    tag  = character(0),
                                    value = character(0),
                                    status = character(0))

    # create data frame for periods
    analysr_env$periods <- tibble::tibble(hash = integer(0),
                                  stat_unit = character(0),
                                  begin = as.POSIXct(NA),
                                  end = as.POSIXct(NA),
                                   tag  = character(0))

    # create data frame for events
    analysr_env$events <- tibble::tibble(hash = integer(0),
                                  stat_unit = character(0),
                                  date = as.POSIXct(NA),
                                  tag  = character(0))

    # create data frame for stat_units
    analysr_env$stat_units <- tibble::tibble(hash = as.integer(0),
                                    stat_unit = character(0))
    # create data frame for descriptions

    analysr_env$descriptions <- tibble::tibble(hash = as.integer(0),
                                    type = character(0),
                                    value = character(0))
    # define current hash used (the first hash to be used will be 1)
    analysr_env$current_hash <- as.integer(0)

    # create data frame for selection
    analysr_env$selection <- tibble::tibble(stat_unit = character(0),
                                    date = as.POSIXct(NA))
    # TODO: change default columns of this df

    # Params: how to convert space to underscore
    analysr_env$space_to_underscore <- FALSE

}
setup_new_env()

#' Get a hash
#'
#' Return a number or vector depending on `n` value
#'
#' @return A number or vector depending on `n` value
#'
#' @param n A number.
#' @examples
#' get_hash(10)
#' @export
get_hash <- function(n = 1) {
    if (n == 1) {
        result <- analysr_env$current_hash + 1
        analysr_env$current_hash <- 1 + analysr_env$current_hash
    } else {
        result <- (analysr_env$current_hash + 1):(n + analysr_env$current_hash)
        analysr_env$current_hash <- n + analysr_env$current_hash
    }
    as.integer(result)
}

#### Intermediate functions used in several functions

stat_unit_from_hash <- function(model, hashs) {
  result <- c()
  for (i in rownames(model$stat_units)) {
    for (j in hashs) {
      if (model$stat_units[i,]$hash == j) {
        result <- c(result, model$stat_units[i,]$stat_unit)
      }
    }
  }
  result
}

hash_from_stat_unit <- function(model, stat_units) {
  result <- c()
  if (length(stat_units) != 0) {
    temp <- dplyr::filter(model$stat_units, stat_unit %in% stat_units)

    temp <- tidyr::pivot_wider(temp,
                                  names_from  = "stat_unit",
                                  values_from = "hash")

    # this function should repeat multiple time
    for (j in stat_units) {
      result <- c(result, temp[toString(j)][[1]])
    }
  }
  result
}

get_duration_from_str <- function(e) {
  e <- tolower(e)

  e <- stringr::str_replace_all(e, "years", "31557600")
  e <- stringr::str_replace_all(e, "year", "31557600")

  e <- stringr::str_replace_all(e, "months", "2678400")
  e <- stringr::str_replace_all(e, "month", "2678400")

  e <- stringr::str_replace_all(e, "weeks", "604800")
  e <- stringr::str_replace_all(e, "week", "604800")

  e <- stringr::str_replace_all(e, "days", "86400")
  e <- stringr::str_replace_all(e, "day", "86400")

  e <- stringr::str_replace_all(e,  "hours", "3600")
  e <- stringr::str_replace_all(e,  "hour", "3600")

  e <- stringr::str_replace_all(e,  "minutes", "60")
  e <- stringr::str_replace_all(e,  "minute", "60")

  e <- stringr::str_replace_all(e,  "seconds", "1")
  e <- stringr::str_replace_all(e,  "second", "1")

  duration_seconds <- eval(str2expression(e))
  return(lubridate::duration(duration_seconds, "seconds"))
}
