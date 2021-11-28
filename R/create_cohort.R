extract_folder <- function(model, unit){
  #The goal of this function is to get every single piece of information we have
  # on this patient and to put it into a big tibble

  folder <- tibble::tibble()

  for (i in rownames(model$stat_units)) {
    if (model$stat_units[i,]$stat_unit == unit) {
      h <- c(result, model$stat_units[i,]$hash)
    }
  }

  #let's check the descriptions table
  temp <- subset(model$descriptions, hash == h)
  n <- nrow(temp)
  hash <- temp$hash
  table <- rep("Descriptions", n)
  tag <- temp$type
  value <- temp$value
  date <- rep(NA, n)
  end <- rep(NA, n)
  folder <- rbind(folder,
                  tibble::tibble(hash, table, tag, value, date, end))

  hashs_to_check <- c()
  #let's check the measures table
  temp <- subset(model$measures, stat_unit == unit)
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

  #let's check the events table
  temp <- subset(model$events, stat_unit == unit)
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

  #let's check the periods table
  temp <- subset(model$periods, stat_unit == unit)
  hash <- temp$hash
  hashs_to_check <- c(hashs_to_check, hash)
  table <- rep("Periods", n)
  tag <- temp$tag
  value <- rep(NA, n)
  date <- temp$begin
  end <- temp$end
  folder <- rbind(folder,
                    tibble::tibble(hash, table, tag, value, date, end))

  #and let's recheck the descriptions table
  hashs_to_keep <- tibble::tibble(hash = hashs_to_check)
  temp <- dplyr::inner_join(model$description, hashs_to_keep, by = "hash")
  n <- nrow(temp)
  hash <- temp$hash
  table <- rep("Descriptions", n)
  tag <- temp$type
  value <- temp$value
  date <- rep(NA, n)
  end <- rep(NA, n)
  folder <- rbind(folder,
                  tibble::tibble(hash, table, tag, value, date, end))


  folder
}



#' create_cohort
#'
#' @param model An AnalysR model
#' @param tag A tag
#'
#' @export
create_cohort <- function(model, tag) {

  cohort <- c()
  new_model <- restrict(model, tag)
  for (s in model$stat_units){

    for (i in rownames(model$stat_units)) {
      if (model$stat_units[i,]$stat_unit == unit) {
        h <- c(result, model$stat_units[i,]$hash)
      }
    }
    folder <- extract_folder(new_model, s)
    cohort <- list(cohort, list(hash = h, stat_unit = s, folder = folder))

  }

  cohort

}
