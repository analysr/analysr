
# on devra implémenter des variables globales

# version basique
import_envperiod_csv <- function (csv_path) {

  result <- readr::read_csv(file=csv_path)
  result <- as.data.frame(result)
  colnames(result) <- c("STAT_UNIT", "BEGIN", "END")
  elocal$period <- rbind(elocal$period, result)
  
}

# avec choix des tags
import_envperiod_csv_2 <- function (csv_path, stat_unit_label = "STAT_UNIT", begin_label = "BEGIN", end_label = "END") {

  result <- readr::read_csv(file=csv_path)
  result <- as.data.frame(result)

  result <- result[c(stat_unit_label,begin_label,end_label)]
  # https://stackoverflow.com/questions/10085806/extracting-specific-columns-from-a-data-frame
  # on pourrait utiliser dplyr pour extraire des colonnes
  colnames(result) <- c("STAT_UNIT", "BEGIN", "END")
  elocal$period <- rbind(elocal$period, result)
}
