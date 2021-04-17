import_events_csv <-
  function (csv_path,
            stat_unit = "stat_unit",
            date = "date",
            tag = "tag",
            ...) {
    quiet_read_csv <- purrr::quietly(readr::read_csv)

    result <- quiet_read_csv(file = csv_path)$result
    result <- as.data.frame(result)

    

    result <- result[c(stat_unit, date, tag)]
    # we could use dplyr to extract colums https://bit.ly/32lGkNR
    colnames(result) <- c("stat_unit", "date", "tag")

    add_stat_units(result$stat_unit)

    result <- cbind(
      hash = get_hash(nrow(result)),
      result
    )

    analysr_env$events <- rbind(analysr_env$events, result)
    result
  }
