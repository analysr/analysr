import_events_csv <-
  function (csv_path,
            stat_unit = "stat_unit",
            date = "date",
            tag = "tag",
            optional_data) {
    quiet_read_csv <- purrr::quietly(readr::read_csv)

    result_csv <- quiet_read_csv(file = csv_path)$result
    result_csv <- as.data.frame(result_csv)

    result <- result_csv[c(stat_unit, date, tag)]
    # we could use dplyr to extract colums https://bit.ly/32lGkNR
    colnames(result) <- c("stat_unit", "date", "tag")

    add_stat_units(result$stat_unit)

    hash <- get_hash(nrow(result))

    result <- cbind(
      hash = hash,
      result
    )

    # using missing https://bit.ly/2QJJyb6
    if (!missing(optional_data)) {
      fill_descriptions(hash, optional_data, result_csv)
    }

    analysr_env$events <- rbind(analysr_env$events, result)
    result
  }
