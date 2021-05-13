import_measures_csv <-
  function(csv_path,
            stat_unit = "stat_unit",
            date = "date",
            tag = "tag",
            value = "value",
            optional_data,
            status = "status") {
    quiet_read_csv <- purrr::quietly(readr::read_csv)

    result <- quiet_read_csv(file = csv_path)$result
    result <- as.data.frame(result)

    n <- nrow(result)
    hash <- get_hash(n)

    if (!("status" %in% colnames(result))) {
      result <- cbind(
        result,
        status = rep("", n)
      )
    } 

    if (!missing(optional_data)) {
      fill_descriptions(hash,optional_data, result,n)
    }

    result <- result[c(stat_unit, date, tag, value, status)]
    # we could use dplyr to extract colums https://bit.ly/32lGkNR
    colnames(result) <- c("stat_unit", "date", "tag", "value", "status")

    add_stat_units(result$stat_unit)

    result$date <- lubridate::ymd_hms(result$date)

    result <- cbind(
      hash,
      result
    )

    analysr_env$measures <- rbind(analysr_env$measures, result)
    result
  }
