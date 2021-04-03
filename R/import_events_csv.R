import_events_csv <-
  function (csv_path,
            stat_unit = "stat_unit",
            date = "date",
            tag = "tag") {
    quiet_read_csv <- purrr::quietly(readr::read_csv)

    result <- quiet_read_csv(file = csv_path)$result
    result <- as.data.frame(result)

    result <- result[c(stat_unit, date, tag)]
    # https://stackoverflow.com/questions/10085806/extracting-specific-columns-from-a-data-frame
    # on pourrait utiliser dplyr pour extraire des colonnes
    colnames(result) <- c("stat_unit", "date", "tag")
    analysr_env$events <- rbind(analysr_env$events, result)
    result
  }
