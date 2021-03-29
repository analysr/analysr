import_periods_csv <- function (csv_path, stat_unit = "stat_unit", begin = "begin", end = "end", desc = "desc"){

  quiet_read_csv <- purrr::quietly(readr::read_csv)

  result <- quiet_read_csv(file=csv_path)$result
  result <- as.data.frame(result)

  result <- result[c(stat_unit,begin,end,desc)]
  # https://stackoverflow.com/questions/10085806/extracting-specific-columns-from-a-data-frame
  # on pourrait utiliser dplyr pour extraire des colonnes
  colnames(result) <- c("stat_unit", "begin", "end", "desc")
  analysr_env$periods <- rbind(analysr_env$periods, result)
  result
}
