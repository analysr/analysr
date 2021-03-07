import <- function (isfile, data, filepath, filetype) {
  # idée, à voir
}
import_CSV <- function (csv_path) {
  # donner les colonnes et les tag à récuperer
  # import measure/period/event


  result <- readr::read_csv(file=csv_path)
  # Opérations de transformation vers un type défini
  #print(typeof(result))
  as.data.frame(result)
}
