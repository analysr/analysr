import_measures_csv <-
  function(csv_path,
            stat_unit = "stat_unit",
            date = "date",
            tag = "tag",
            value = "value",
            optional_data) {
    quiet_read_csv <- purrr::quietly(readr::read_csv)

    result <- quiet_read_csv(file = csv_path)$result
    result <- as.data.frame(result)

    n <- nrow(result)
    hash <- get_hash(n)

    if (!missing(optional_data)) {
      fill_descriptions(hash, optional_data, result, n)
    }

    result <- result[c(stat_unit, date, tag, value)]
    # we could use dplyr to extract colums https://bit.ly/32lGkNR
    colnames(result) <- c("stat_unit", "date", "tag", "value")

    add_stat_units(result$stat_unit)


    result <- cbind(
      hash,
      result
    )

    analysr_env$measures <- rbind(analysr_env$measures, result)
    result
  }
