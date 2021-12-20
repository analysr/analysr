
library(tidyverse)
library(tictoc)

setup_new_env()

tic("Import measures")
import_measures_csv(
    csv_path = "./benchmark/csv_100/measures_100.csv",
    stat_unit = "PATIENT",
    date = "DATE",
    tag = "DESCRIPTION",
    value = "VALUE")
toc()

tic("Import events")
import_events_csv(
    csv_path = "./benchmark/csv_100/events_100.csv",
    stat_unit = "PATIENT",
    date = "DATE",
    tag = "DESCRIPTION")
toc()

tic("Import periods")
import_periods_csv(
    csv_path = "./benchmark/csv_100/periods_100.csv",
    stat_unit = "PATIENT",
    begin = "START",
    end = "STOP",
    tag = "DESCRIPTION")
toc()

tic("Import stat_units")
import_stat_units_csv(
    csv_path = "./benchmark/csv_100/patients_100.csv",
    stat_unit = "UserId",
    optional_data = c("BIRTHDATE","DEATHDATE","FIRST","LAST","RACE","ETHNICITY","GENDER","STATE","HEALTHCARE_EXPENSES","HEALTHCARE_COVERAGE"))
toc()

tic("Request")
result <- (
  analysr_env
  %>% observed(`Body Weight` < 80)
  %>% before(`Combined chemotherapy and radiation therapy (procedure)`)
)
toc()

# Answer here

sprintf("Nombre de données trouvées: %d",nrow(result$selection))
if (nrow(result$selection)>0){
  print("Exemple d'élément trouvé")
  print(result$selection[1,])
}
