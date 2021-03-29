

import_measures_csv <- function (csv_path, stat_unit = "stat_unit", date = "date", tag = "tag", value = "value"){


  result <- readr::read_csv(file=csv_path)
  result <- as.data.frame(result)

  result <- result[c(stat_unit,date,tag,value)]
  # https://stackoverflow.com/questions/10085806/extracting-specific-columns-from-a-data-frame
  # on pourrait utiliser dplyr pour extraire des colonnes
  colnames(result) <- c("stat_unit", "date", "tag", "value")
  analysr_env$measures<-rbind(analysr_env$measures, result)
  result
}
