


import_measures_csv <-
  function (csv_path,
            stat_unit = "stat_unit",
            date = "date",
            tag = "tag",
            value = "value") {
    quiet_read_csv <- purrr::quietly(readr::read_csv)

    result <- quiet_read_csv(file = csv_path)$result
    result <- as.data.frame(result)

    result <- result[c(stat_unit, date, tag, value)]
    # https://stackoverflow.com/questions/10085806/extracting-specific-columns-from-a-data-frame
    # on pourrait utiliser dplyr pour extraire des colonnes
    colnames(result) <- c("stat_unit", "date", "tag", "value")

    add_stat_units(result$stat_unit)

    result <- cbind(
      hash = get_hash(nrow(result)),
      result
    )




    analysr_env$measures <- rbind(analysr_env$measures, result)
    result
  }
