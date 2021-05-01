import_periods_csv <-
  function(csv_path,
            stat_unit = "stat_unit",
            begin = "begin",
            end = "end",
            desc = "desc",
            optional_data) {
    quiet_read_csv <- purrr::quietly(readr::read_csv)

    result <- quiet_read_csv(file = csv_path)$result
    result <- as.data.frame(result)

    n <- nrow(result)
    hash <- get_hash(n)

    if (!missing(optional_data)) {
      fill_descriptions(hash, optional_data, result, n)
    }

    result <- result[c(stat_unit, begin, end, desc)]
    # we could use dplyr to extract colums https://bit.ly/32lGkNR
    colnames(result) <- c("stat_unit", "begin", "end", "desc")

    add_stat_units(result$stat_unit)

    result <- cbind(
      hash,
      result
    )

    analysr_env$periods <- rbind(analysr_env$periods, result)
    result
  }
