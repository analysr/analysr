



#' restrict
#'
#' @param env An AnalysR model
#' @param condition A condition
#'
#' @export
restrict <- function(env, condition){

#the goal is to restrict the model to patients that have a certain condition

#we suppose that the condition is in descriptions



  #let's setup a new environment

#------------------------------------------------------------------------------

  model <- new.env(parent = emptyenv())

  # create query
  model$query <- list()

  # create data frame for measures
  model$measures <- tibble::tibble(hash = integer(0),
                                         stat_unit = character(0),
                                         date = as.POSIXct(NA),
                                         tag  = character(0),
                                         value = character(0),
                                         status = character(0))

  # create data frame for periods
  model$periods <- tibble::tibble(hash = integer(0),
                                        stat_unit = character(0),
                                        begin = as.POSIXct(NA),
                                        end = as.POSIXct(NA),
                                        tag  = character(0))

  # create data frame for events
  model$events <- tibble::tibble(hash = integer(0),
                                       stat_unit = character(0),
                                       date = as.POSIXct(NA),
                                       tag  = character(0))

  # create data frame for stat_units
  model$stat_units <- tibble::tibble(hash = as.integer(0),
                                           stat_unit = character(0))
  # create data frame for descriptions

  model$descriptions <- tibble::tibble(hash = as.integer(0),
                                             type = character(0),
                                             value = character(0))
  # define current hash used (the first hash to be used will be 1)
  model$current_hash <- as.integer(0)

  # create data frame for selection
  model$selection <- tibble::tibble(stat_unit = character(0),
                                          date = as.POSIXct(NA))

#------------------------------------------------------------------------------


  #Now that we have an empty environment, let's add all the data we want

  #we'll start by getting all the hashs that match with the condition
  # we suppose for now that the condition is a description about a stat_unit

  condition <- rlang::enexpr(condition)
  hashs_to_check <- c()

  if (length(condition) > 2) {
    # Method with operator
    # e.g. Temperature > 38.5

    operator <- condition[[1]]
    if (is.symbol(condition[[3]])) {
      # let's select the hashs that have the query condition
      # the list will be in stocked in hashs_to_check
      tag_to_check <- condition[[3]]
      rvalue <- condition [[2]]

      # Check on descriptions table
      temp <- subset(env$descriptions, type == tag_to_check)
      temp <- temp[eval(rlang::call2(operator, rvalue,
                                     convert_to_best_type(temp$value))),]
      if (nrow(temp) != 0) {
        hashs_to_check <- c(hashs_to_check, temp$hash)
      }

    } else {
      tag_to_check <- condition[[2]]
      rvalue <- condition [[3]]
        # Check on descriptions table
      temp <- subset(env$descriptions, type == tag_to_check)
      temp <- temp[eval(rlang::call2(operator,
                                     convert_to_best_type(temp$value), rvalue)),]

      if (nrow(temp) != 0) {
        hashs_to_check <- c(hashs_to_check, temp$hash)
      }
    }

  } else {
    # Method without operator
    tag_to_check <- condition

    # Check on descriptions table
    temp <- subset(env$descriptions, type == tag_to_check)
    if (nrow(temp) != 0) {
      hashs_to_check <- c(hashs_to_check, temp$hash)
      }
  }
  hashs_to_keep <- tibble::tibble(hash = hashs_to_check)
#------------------------------------------------------------------------------
  #Now we have all the hash we want in hashs_to_keep
  #let's see to what entries they lead and let's add these entries to the model


  # Check on stat_unit table
  temp <- dplyr::inner_join(env$stat_units, hashs_to_keep, by = "hash")
  if ((n <- nrow(temp)) != 0) {
    model$stat_units <- rbind(model$stat_units, temp)
  }
  #Check on measures table
  temp <- dplyr::inner_join(env$measures, hashs_to_keep, by = "hash")
  if ((n <- nrow(temp)) != 0) {
    wanted_stat_units <- temp$stat_unit
    wanted_hashs <- hash_from_stat_unit(env,wanted_stat_units)
    add <- tibble::tibble(hash = wanted_hashs, stat_unit = wanted_stat_units)
    model$stat_units <- rbind(model$stat_units, add)
  }
  #Check on events table
  temp <- dplyr::inner_join(env$events, hashs_to_keep, by = "hash")
  if ((n <- nrow(temp)) != 0) {
    wanted_stat_units <- temp$stat_unit
    wanted_hashs <- hash_from_stat_unit(env,wanted_stat_units)
    add <- tibble::tibble(hash = wanted_hashs, stat_unit = wanted_stat_units)
    model$stat_units <- rbind(model$stat_units, add)
  }
  #Check on periods table
  temp <- dplyr::inner_join(env$periods, hashs_to_keep, by = "hash")
  if ((n <- nrow(temp)) != 0) {
    wanted_stat_units <- temp$stat_unit
    wanted_hashs <- hash_from_stat_unit(env,wanted_stat_units)
    add <- tibble::tibble(hash = wanted_hashs, stat_unit = wanted_stat_units)
    model$stat_units <- rbind(model$stat_units, add)
  }


  # Let's now add all the entries that concern those stat_unit
  model$measures <- rbind(model$measures, dplyr::filter(env$measures,
                                  stat_unit %in% model$stat_units$stat_unit))
  model$events <- rbind(model$events, dplyr::filter(env$events,
                                  stat_unit %in% model$stat_units$stat_unit))
  model$periods <- rbind(model$periods, dplyr::filter(env$periods,
                                  stat_unit %in% model$stat_units$stat_unit))
  model$descriptions <- rbind(model$description,
                                         dplyr::filter(env$descriptions,
                                        hash %in% model$stat_units$hash))


  model$descriptions <- rbind(model$descriptions,
                                    dplyr::filter(env$descriptions,
                                    hash %in% model$measures$hash))

  model$descriptions <- rbind(model$descriptions,
                                    dplyr::filter(env$descriptions,
                                    hash %in% model$events$hash))

  model$descriptions <- rbind(model$descriptions,
                                  dplyr::filter(env$descriptions,
                                   hash %in% model$periods$hash))













  # Check on measures table
  temp <- dplyr::inner_join(env$measures, hashs_to_keep, by = "hash")
  if ((n <- nrow(temp)) != 0) {
    model$measures <- rbind(model$measures, temp)
  }

  # Check on events table
  temp <- dplyr::inner_join(env$events, hashs_to_keep, by = "hash")
  if ((n <- nrow(temp)) != 0) {
    model$events <- rbind(model$events, temp)
  }

  # Check on periods table
  temp <- dplyr::inner_join(env$periods, hashs_to_keep, by = "hash")
  if ((n <- nrow(temp)) != 0) {
    model$periods <- rbind(model$periods, temp)
  }

  model$current_hash <- env$current_hash

  model
}
