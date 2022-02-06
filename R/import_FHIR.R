#' import_fhir
#'
#' Import data from a FHIR folder
#'
#' @return Merge of imported data and already imported data
#'
#' @param folder_path A path to the FHIR file.
#' @param date A vector of labels containing dates.
#' @param begin A vector of labels containing begin dates.
#' @param end A vector of labels containing end dates.
#' @param tag A vector of labels containing tags.
#' @param value A vector of labels containing values.
#'
#' @export
import_fhir <-
  function(folder_path,
           date = c("date"),
           begin = c("begin"),
           end = c("end"),
           tag = c("tag"),
           value = c("value")) {
    home_dir <- getwd()
    setwd(folder_path) # set working directory in the FHIR folder

    l <- list.files(pattern = ".json") # make a list of the names of FHIR files

    for (i in l) {
      t <- strsplit(i, split = "_")
      name <- paste(t[[1]][[1]], t[[1]][[2]]) # keep the name of the patient

      result <- jsonlite::fromJSON(i) # import data from JSON
      data_raw <- tibble::enframe(unlist(result)) # store data in two columns
      result <- tidyr::separate(data_raw, name,
        into = c(paste0("x", 1:10)),
        fill = "right"
      ) # store data in 11 columns
      n <- nrow(data_raw) # get row number
      cev <- 0 # number of events added
      cme <- 0 # number of measures added
      cpe <- 0 # number of periods added
      events <- tibble::tibble(stat_unit = NULL, date = NULL, tag = NULL)
      periods <- tibble::tibble(
        stat_unit = NULL, begin = NULL,
        end = NULL, tag = NULL
      )
      measures <- tibble::tibble(
        stat_unit = NULL, date = NULL,
        tag = NULL, value = NULL, status = NULL
      )
      for (j in 1:n) {
        for (t in tag) {
          if (grepl(t, result[[11]][[j]])) {
            # checking if tag is found
            p <- TRUE # TRUE if we need to check for a begin date
            a <- TRUE # TRUE if we need to check for an end date
            ap <- TRUE # FALSE if we have found an end date
            bp <- TRUE # False if we have found a begin date
            da <- TRUE # TRUE if we need to check for a date
            ev <- TRUE # TRUE if we have found a date
            me <- TRUE # TRUE if we have found a measure
            v <- result[[11]][[j]] # keeping tag
            jp <- 1 # store row value
            reso <- TRUE
            for (column in 10:1) {
              # keeping the resource
              if ((grepl("resourceType", result[[column]][[j]]))) {
                h <- column
                r <- result[[column]][[j]]
                id <- substring(r, 13, nchar(r))
                reso <- FALSE
              }
              if ((column == 1) & reso) {
                jp <- n
              }
            }
            while ((jp < n) & ((ap & bp) | me)) {
              jp <- jp + 1
              if (!is.na(result$value[[jp]])) {
                hp <- 1 # store column value
                while ((hp < 10) & (!is.na(result[[hp + 1]][[jp]]))) {
                  hp <- hp + 1
                  if (id == (regmatches(
                    result[[hp]][[jp]],
                    gregexpr("[[:digit:]]+", result[[hp]][[jp]])
                  ))) {
                    # looking for the same id
                    if (p) {
                      for (b in begin) { # looking for a begin date
                        if (grepl(b, (result[[hp]][[jp]]))) {
                          d <- result[[11]][[jp]]
                          p <- FALSE
                          bp <- FALSE
                          da <- FALSE
                        }
                      }
                    }
                    if (a) { # looking for an end date
                      for (e in end) {
                        if (grepl(e, (result[[hp]][[jp]]))) {
                          c <- result[[11]][[jp]]
                          a <- FALSE
                          ap <- FALSE
                          da <- FALSE
                        }
                      }
                    }
                    if (da) { # looking for a date
                      for (co in date) {
                        if (grepl(co, (result[[hp]][[jp]]))) {
                          edate <- result[[11]][[jp]]
                          da <- FALSE
                          ev <- FALSE
                          a <- FALSE
                          b <- FALSE
                        }
                      }
                    }
                    if (!ev | da) { # looking for a measure
                      for (mea in value) {
                        if (grepl(mea, (result[[hp]][[jp]]))) {
                          meas <- result[[11]][[jp]]
                          me <- FALSE
                          ev <- TRUE
                        }
                      }
                    }
                  }
                }
              }
            }

            if (!bp & !ap) { # adding a period
              periods <- rbind(periods, c(name, d, c, v))
              cpe <- cpe + 1
            }
            if (!da & !ev) { # adding an event
              events <- rbind(events, c(name, edate, v))
              cev <- cev + 1
            }
            if (!da & !me) { # adding a measure
              measures <- rbind(measures, c(name, edate, v, meas, NA))
              cme <- cme + 1
            }
          }
        }
      }
      if (cpe != 0) {
        colnames(periods) <- c("stat_unit", "begin", "end", "tag")
        import_periods_df(periods,
          date_format_func = lubridate::ymd_hms,
          force_date_format = TRUE, model = analysr_env
        )
      }

      if (cme != 0) {
        colnames(measures) <- c("stat_unit", "date", "tag", "value", "status")
        import_measures_df(measures,
          date_format_func = lubridate::ymd_hms,
          force_date_format = TRUE, model = analysr_env
        )
      }

      if (cev != 0) {
        colnames(events) <- c("stat_unit", "date", "tag")
        import_events_df(events,
          date_format_func = lubridate::ymd_hms,
          force_date_format = TRUE, model = analysr_env
        )
      }
    }
    setwd(home_dir)
  }