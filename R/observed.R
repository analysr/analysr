stat_unit_from_hash <- function(hashs) {
  result <- c()
  for (i in rownames(analysr_env$stat_units)) {
    for (j in hashs) {
      if (analysr_env$stat_units[i,]$hash == j) {
        result <- c(result, analysr_env$stat_units[i,]$stat_unit)
      }
    }
  }
  result
}

hash_from_stat_unit <- function(model, stat_units) {
  result <- c()
  if (length(stat_units) != 0) {
    for (i in rownames(model$stat_units)) {
      for (j in stat_units) {
        if (model$stat_units[i,]$stat_unit == j) {
          result <- c(result, model$stat_units[i,]$hash)
        }
      }
    }
  }

  result
}

check_integer <- function(n) {
    !grepl("[^[:digit:]]", format(n,  digits = 20, scientific = TRUE))
}

convert_to_best_type <- function(vect) {
    to_convert <- FALSE

    for (i in vect) {
       if (check_integer(i)) {
        to_convert <- TRUE
        break
      }
    }
    if (to_convert) {
      suppressWarnings(as.numeric(vect))
    } else {
      vect
    }
}

get_entries_from_hash <- function (model, preselection) {
    result <- tibble::tibble()

    # Check on stat_unit table
    temp <- dplyr::inner_join(model$stat_units, preselection, by = "hash")
    if ((n <- nrow(temp)) != 0) {
      stat_unit <- temp$stat_unit
      date_obs <- rep(NA, n)
      hash_stat_unit <- temp$hash
      hash_obs <- rep(NA, n)
      result <- rbind(result,
                  tibble::tibble(hash_stat_unit, stat_unit, hash_obs, date_obs))
    }

    # Check on measures table
    temp <- dplyr::inner_join(model$measures, preselection, by = "hash")
    if (nrow(temp) != 0) {
      stat_unit <- temp$stat_unit
      date_obs <- temp$date
      hash_stat_unit <- hash_from_stat_unit(model, temp$stat_unit)
      hash_obs <- temp$hash
      result <- rbind(result,
                  tibble::tibble(hash_stat_unit, stat_unit, hash_obs, date_obs))
    }

    # Check on events table
    temp <- dplyr::inner_join(model$events, preselection, by = "hash")
    if (nrow(temp) != 0) {
      stat_unit <- temp$stat_unit
      date_obs <- temp$date
      hash_stat_unit <- hash_from_stat_unit(model, temp$stat_unit)
      hash_obs <- temp$hash
      result <- rbind(result,
                  tibble::tibble(hash_stat_unit, stat_unit, hash_obs, date_obs))
    }

    # Check on periods table
    temp <- dplyr::inner_join(model$periods, preselection, by = "hash")
    if (nrow(temp) != 0) {
      stat_unit <- temp$stat_unit
      date_obs <- temp$begin
      hash_stat_unit <- hash_from_stat_unit(model, temp$stat_unit)
      date_obs_end <- temp$end
      hash_obs <- temp$hash
      result <- rbind(result, tibble::tibble(hash_stat_unit, stat_unit,
                                             hash_obs, date_obs, date_obs_end))
    }
    result
}

prepare_query <- function(model, condition) {
  selection <- tibble::tibble()
  if (length(condition) > 2) {
    # Method with operator
    # Here we admit that a condition is like: tag operator value
    # e.g. Temperature > 38.5

    #if there's an operator, the information will be in the measure table

    operator <- condition[[1]]
    if (is.symbol(condition[[3]])) {
      # let's select the stat_units that have the query condition
      # the list will be in stocked in query$stat_units_selected
      tag_to_check <- condition[[3]]
      rvalue <- condition [[2]]

      # Check on measures table
      temp <- subset(model$measures, tag == tag_to_check)
      temp <- temp[eval(rlang::call2(operator, rvalue, temp$value)),]
      stat_unit <- temp$stat_unit
      date_obs <- temp$date
      hash_stat_unit <- hash_from_stat_unit(model, temp$stat_unit)
      hash_obs <- temp$hash
      selection <- rbind(selection,
                  tibble::tibble(hash_stat_unit, stat_unit, hash_obs, date_obs))

      # Check on descriptions table
      temp <- subset(model$descriptions, type == tag_to_check)
      temp <- temp[eval(rlang::call2(operator, rvalue,
                                    convert_to_best_type(temp$value))),]
      if (nrow(temp) != 0) {
        selection <- rbind(selection, get_entries_from_hash(model, temp))
      }

    } else {

      # let's select the stat_units that have the query condition
      # the list will be in stocked in query$stat_units_selected
      tag_to_check <- condition[[2]]
      rvalue <- condition [[3]]


      # Check on measures table
      temp <- subset(model$measures, tag == tag_to_check)
      temp <- temp[eval(rlang::call2(operator, temp$value, rvalue)),]
      stat_unit <- temp$stat_unit
      date_obs <- temp$date
      hash_stat_unit <- hash_from_stat_unit(model, temp$stat_unit)
      hash_obs <- temp$hash
      selection <- rbind(selection,
                  tibble::tibble(hash_stat_unit, stat_unit, hash_obs, date_obs))


      # Check on descriptions table
      temp <- subset(model$descriptions, type == tag_to_check)
      temp <- temp[eval(rlang::call2(operator,
                        convert_to_best_type(temp$value), rvalue)),]

      if (nrow(temp) != 0) {
        selection <- rbind(selection, get_entries_from_hash(model, temp))
      }
    }

  } else {
    # Method without operator
    # When there is no operator, check events or description,
    # measures with description (damn hard)

    tag_to_check <- condition

    # Check on events table
    temp <- subset(model$events, tag == tag_to_check)
    stat_unit <- temp$stat_unit
    date_obs <- temp$date
    hash_stat_unit <- hash_from_stat_unit(model, temp$stat_unit)
    hash_obs <- temp$hash
    selection <- rbind(selection,
                  tibble::tibble(hash_stat_unit, stat_unit, hash_obs, date_obs))

    # Check on periods table
    temp <- subset(model$periods, tag == tag_to_check)
    stat_unit <- temp$stat_unit
    hash_stat_unit <- hash_from_stat_unit(model, temp$stat_unit)
    date_obs <- temp$begin
    date_obs_end <- temp$end
    hash_obs <- temp$hash
    selection <- rbind(selection,
    tibble::tibble(hash_stat_unit, stat_unit, hash_obs, date_obs, date_obs_end))

    # Check on descriptions table
    temp <- subset(model$descriptions, type == tag_to_check)
    if (nrow(temp) != 0) {
        selection <- rbind(selection, get_entries_from_hash(model, temp))
    }

  }
  selection
}

#' observed
#'
#' @param model An AnalysR model
#' @param condition A condition
#'
#' @export
observed <- function(model, condition) {
  condition <- rlang::enexpr(condition)

  model$selection <- model$selection[0,] # here we reset the selection
  model$query <- list() # here we reset the query
  model$query$condition <- condition

  model$selection <- prepare_query(model, condition)

  if (length(condition) > 2) {
    if (is.symbol(condition[[3]])) {
      model$query$tag <- rlang::as_string(condition[[3]])[1]
    } else {
      model$query$tag <- rlang::as_string(condition[[2]])[1]
    }
  } else {
    model$query$tag <- rlang::as_string(condition)
  }

  model
}
