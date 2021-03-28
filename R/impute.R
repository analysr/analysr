library(lubridate)

import_CSV <- function (csv_path) {
  # donner les colonnes et les tag à récuperer

  result <- readr::read_csv(file=csv_path)
  # Opérations de transformation vers un type défini
  #print(typeof(result))
  print(result)
  as.data.frame(result)
}

seq_date <- function (begin, end, freq) {
  begin <- lubridate::as_date(begin)
  end <- lubridate::as_date(end)

  #Création d'un interval
  i <- lubridate::interval(begin, end)

  nb_pts <- floor(i / freq)
  begin + 0:nb_pts * freq
}

impute <- function(data, tag, period_start, period_end, temporal_granularity, stat_unit = NULL, method = 'mean', information_lost_after = 5*temporal_granularity) {
  # Modification à apporter:
  # Alerte quand les données ne sont pas assez fournies

  # Méthode
  # Il s'agit de créer une liste avec les dates souhaitées. Pour chaque date,soit une valeur correspond. Sinon on prend la moyenne de la plus proche après et la plus proche avant

  print("Début de impute")

  #On filtre les element selon 3 critères
  data <- subset(data, TAG==tag)
  data <- subset(data, DATE < period_end)
  data <- subset(data, DATE > period_start)

  # création  des différents intervals
  d <- as.POSIXlt(period_start)
  d$hour <- d$hour + 24
  dateInterval <- lubridate::interval (period_start,d)
  dateList <- list(dateInterval)

  #verifier ca
  while (int_end(dateInterval) < period_end){

    #Mauvaise méthode ... je n'arrive pas à trouver de solution
    l <- list(dateInterval)
    dateList <- append(dateList,l)
    dateInterval <- int_shift(dateInterval, duration(days = 1))
    #print(dateInterval)
  }

  valueList <-  list()
  idList <- list()

  good <- 0
  notGood <- 0
  notEnough <- 0

  for (i in 1:length(dateList)){
    #Pour chaque date
    funded <-0
    closestAfterIndex <- 1
    closestBeforeIndex <- 1

    # data[j][2] -> date
    # data[j][4] -> valeur
    # data[j][1] -> id

    # tag et stat_unit aussi
    # On chercher si une date correspond et l'index de la plus proche
    for (j in 1:length(data)){
      #verifier qu'il s'agit bien de la colonne des dates
      if (data[j,2]  %within% dateList[[i]] == TRUE){
        if (funded == 0){
          good <- good+1
          valueList <- append(valueList,data[j,4])
          idList <- append(idList,data[j,1])
        }
        funded <-1

      }
      #Juste avant la borne inf mais après la meilleure valeur trouvée
      if( (data[j,2] < int_start(dateList[[i]]) )&& (data[j,2] > data[closestBeforeIndex,2]) ){
        closestBeforeIndex <- j
      }
      if(data[j,2] > int_end(dateList[[i]] )&& data[j,2] < data[closestAfterIndex,2] ){
        closestAfterIndex <- j
      }
    }

    #Si aucun date ne correspond on complète..
    # On verifie que les dates choisies ne sont pas trop loin de la date d'origine

    if (funded == 0){
      notGood<- notGood+1
      print(data[closestAfterIndex,2]- int_end(dateList[[i]]))

      #verification après date de fin
      if (data[closestAfterIndex,2]- int_end(dateList[[i]]) > 3){
        notEnough <-notEnough+ 1
      }
      if (data[closestAfterIndex,2]- int_end(dateList[[i]]) < 0){
        notEnough <-notEnough+ 1
      }

      #verification avant date de début
      if (int_start(dateList[[i]]) -data[closestBeforeIndex,2] > 3){
        notEnough <-notEnough+ 1
      }
      if (int_start(dateList[[i]]) -data[closestBeforeIndex,2] < 0){
        notEnough <-notEnough+ 1
      }
      if(method == "mean"){
        #MOYENNE ENTRE LA DATE PLUS PROCHE AVANT ET LA DATE LA PLUS PROCHE APRES
        #On prend 1 pour avoir la valeur et non la date
        value <- (data[closestAfterIndex,4] +  data[closestBeforeIndex,4])/2
        valueList <- append(valueList,value)
      }
      else if(method == "max"){
        #MAX ENTRE LA VALEUR DE LA DATE PLUS PROCHE AVANT ET DE LA VALEUR DE LA DATE LA PLUS PROCHE APRES
        value <- max(c(data[closestAfterIndex,4] ,  data[closestBeforeIndex,4]) )
        valueList <- append(valueList,value)
      }
      else if(method == "min"){
        value <- min(c(data[closestAfterIndex][4] ,  data[closestBeforeIndex][4]) )
        valueList <- append(valueList,value)
      }
      else if(method == "mediane"){
        #valueMoy <-
        #push(valueList,valueMoy)
        print("La methode mediane n'a pas été codée")
      }
      else {
        print("La méthode n'existe pas")
        break
      }
    }
  }

  print("Programme terminé, nombre d'intervales générés: ")
  print(length (dateList))
  print("Nombre de valeurs trouvées")
  print(length(valueList))
  print("Nombre de valeurs originales (non modifiées):")
  print(good)
  print("Nombre de valeurs déduite..:")
  print(notGood)
  if (notEnough > 1){
    pourcentage = 100*notEnough/(2*(notGood))
    print("Le pourcentage de données non coherente vaut: ")
    print(pourcentage)
  }
}
impute(import_CSV("../tests/testthat/set.csv"), "Kaliemie", "2006-11-23 12:00", "2006-12-18 12:00", "day")


