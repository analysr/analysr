import_envmeasures_csv <- function (csv_path, id = "patient", date = "date_prlvt", tag = "type_examen", value = "valeur"){
  
  
  result <- readr::read_csv(file=csv_path)
  result <- as.data.frame(result)
  
  result <- result[c(id,date,tag,value)]
  # https://stackoverflow.com/questions/10085806/extracting-specific-columns-from-a-data-frame
  # on pourrait utiliser dplyr pour extraire des colonnes
  colnames(result) <- c("id", "date", "tag", "value")
  
  elocal$measures <- rbind(elocal$measures, result)
}