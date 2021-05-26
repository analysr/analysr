description_query <-
  function(wanted_value, wanted_type) {
    if (!missing(type)){
      wanted_hash <- subset(analysr_env$descriptions, type == wanted_type)
      wanted_hash <- subset(wanted_hash, value == wanted_value)

      for (h in wanted_hash){
        wanted_measures <- rbind(wanted_measures,
                                 subset(analysr_env$measures, hash == h))
        wanted_periods <- rbind(wanted_periods,
                                subset(analysr_env$periods, hash == h))
        wanted_events <- rbind(wanted_events,
                               subset(analysr_env$events, hash == h))
      }
      col <- c(hash, type, stat_unit, begin, end, tag, value)

      hash <- wanted_measures$hash
      hash <- c(hash, wanted_periods$hash)
      hash <- c(hash, wanted_events$hash)

      type <- rep("measure", length(wanted_measures))
      type <- c(type, rep("period", length(wanted_periods)))
      type <- c(type, rep("event", length(wanted_events)))

      stat_unit <- wanted_measures$stat_unit
      stat_unit <- c(stat_units, wanted_periods$stat_unit)
      stat_unit <- c(stat_units, wanted_events$stat_unit)

      begin <- wanted_measures$date
      begin <- c(begin, wanted_periods$begin)
      begin <- c(begin, wanted_events$date)

      end <- rep(NA, length(wanted_measures))
      end <- c(end, wanted_periods$end)
      end <- c(end, rep(NA, length(wanted_events)))

      tag <- wanted_measures$tag
      tag <- c(begin, wanted_periods$desc)
      tag <- c(begin, wanted_events$tag)

      value <- wanted_measures$value
      value <- c(value, rep(NA, length(wanted_periods)))
      value <- c(value, rep(NA, length(wanted_events)))

      return (data.frame(hash, type, stat_unit, begin, end, tag, value))

    }

  }

