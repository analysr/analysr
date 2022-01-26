extract_folder <- function(model, unit) {
  #The goal of this function is to get every single piece of information we have
  # on this patient and to put it into a big tibble

  folder <- tibble::tibble()

  h <- dplyr::filter(model$stat_units, stat_unit == unit)$hash

  #let's check the descriptions table
  temp <- subset(model$descriptions, hash == h)
  if ((n <- nrow(temp)) != 0) {
    n <- nrow(temp)
    hash <- temp$hash
    table <- rep("Descriptions", n)
    tag <- temp$type
    value <- temp$value
    date <- rep(NA, n)
    end <- rep(NA, n)
    folder <- rbind(folder,
                    tibble::tibble(hash, table, tag, value, date, end))
  }

  hashs_to_check <- c()
  #let's check the measures table
  temp <- subset(model$measures, stat_unit == unit)
  if ((n <- nrow(temp)) != 0) {
    n <- nrow(temp)
    hash <- temp$hash
    hashs_to_check <- c(hashs_to_check, hash)
    table <- rep("Measures", n)
    tag <- temp$tag
    value <- temp$value
    date <- temp$date
    end <- rep(NA, n)
    folder <- rbind(folder,
                      tibble::tibble(hash, table, tag, value, date, end))
  }

  #let's check the events table
  temp <- subset(model$events, stat_unit == unit)
  if ((n <- nrow(temp)) != 0) {
    n <- nrow(temp)
    hash <- temp$hash
    hashs_to_check <- c(hashs_to_check, hash)
    table <- rep("Events", n)
    tag <- temp$tag
    value <- rep(NA, n)
    date <- temp$date
    end <- rep(NA, n)
    folder <- rbind(folder,
                      tibble::tibble(hash, table, tag, value, date, end))
  }

  #let's check the periods table
  temp <- subset(model$periods, stat_unit == unit)
  if ((n <- nrow(temp)) != 0) {
    hash <- temp$hash
    hashs_to_check <- c(hashs_to_check, hash)
    table <- rep("Periods", n)
    tag <- temp$tag
    value <- rep(NA, n)
    date <- temp$begin
    end <- temp$end
    folder <- rbind(folder,
                      tibble::tibble(hash, table, tag, value, date, end))
  }

  #and let's recheck the descriptions table
  hashs_to_keep <- tibble::tibble(hash = hashs_to_check)

  temp <- dplyr::inner_join(model$descriptions, hashs_to_keep, by = "hash")
  if ((n <- nrow(temp)) != 0) {
    n <- nrow(temp)
    hash <- temp$hash
    table <- rep("Descriptions", n)
    tag <- temp$type
    value <- temp$value
    date <- rep(NA, n)
    end <- rep(NA, n)
    folder <- rbind(folder,
                    tibble::tibble(hash, table, tag, value, date, end))
  }


  folder
}



#' export_raw
#'
#' The export_raw function is an experimental feature.
#' There is no unit test. This allows you to export data grouped by stat_units.
#'
#' @param model An AnalysR model
#' @param tag A tag
#'
#' @export
export_raw <- function(model, tag) {
  cohort <- c()
  tag <- rlang::enexpr(tag)
  new_model <- restrict(model, tag, catch = FALSE)

  for (s in new_model$stat_units$stat_unit) {

    h <- dplyr::filter(new_model$stat_units, stat_unit == s)$hash
    folder <- extract_folder(new_model, s)
    cohort <- list(cohort, list(hash = h, stat_unit = s, folder = folder))

  }

  cohort

}
