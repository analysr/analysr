library(dplyr)

import_periods_csv <- function (csv_path, stat_unit = "stat_unit", begin = "begin", end = "end", desc = "desc"){


  result <- readr::read_csv(file=csv_path)
  result <- as.data.frame(result)

  result <- result[c(stat_unit,begin,end,desc)]
  # https://stackoverflow.com/questions/10085806/extracting-specific-columns-from-a-data-frame
  # on pourrait utiliser dplyr pour extraire des colonnes
  colnames(result) <- c("stat_unit", "begin", "end", "desc")
  analysr_env$periods<-rbind(analysr_env$periods, result)
  result
}
