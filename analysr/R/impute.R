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
  #subset pour faire des sous ensemble avec un filtre
  print("Début de impute")
  print(period_start)
  print(period_end)

  #On filtre les element avec le bon tag, compris entre la date de début et la date de fin
  data <- subset(data, TAG==tag)
  data <- subset(data, DATE < period_end)
  data <- subset(data, DATE > period_start)

  #Il s'agit de créer une liste avec les dates souhaitées. Pour chaque date,soit une valeur correspond. Sinon on prend la moyenne de la plus proche après et la plus proche avant

  print("test de la boucle")
  for (j in data){
    print(data[j])
  }

  #Methode moyenne
  if (method == "mean") {
    #pour chaque date
    #Création de la liste des dates
    dateList <- lubridate::interval (period_start,period_end)
    valueList <- list()
    idList <- list()
    print("Début de la boucle")
    print(dateList)

    for (i in dateList){
      #Pour chaque date
      funded <-0
      closestAfterIndex <- 0
      closestBeforeIndex <- 0

      # data[j][0] -> date
      # data[j][1] -> valeur
      # data[j][2] -> id

      # tag et stat_unit aussi
      print(dateList[i])
      # On chercher si une date correspond et l'index de la plus proche
      for (j in data){
        #verifier qu'il s'agit bien de la colonne des dates
        if (data[j][0]  %within% dateList[i] == TRUE){
          funded <-1
          push(valueList,data[j][1])
          push(valueList,data[j][2])

        }
        #Juste avant la borne inf mais après la meilleure valeur trouvée
        if(data[j][0] < dateList[i][0] && data[j][0] > data[closestBeforeIndex][0] ){
          closestBeforeIndex <- j
        }
        if(data[j][0] > dateList[i][1] && data[j][0] < data[closestAfterIndex][0] ){
          closestAfterIndex <- j
        }

      }

      #Si aucun date ne correspond
      if (funded == 0){
        #On prend 1 pour avoir la valeur et non la date
        valueMoy <- (data[closestAfterIndex][1] +  data[closestBeforeIndex][1])/2
        push(valueList,valueMoy)
      }

    }


  }

  #dates
  if (temporal_granularity == "day") {
    print(analysrr::seq_date(period_start,period_end,1))
  }

  print(data)


  # import(temporaire)
  data
}
impute(import_CSV("./set.csv"), "Kaliemie", "2006-01-01 12:00:00", "2006-12-31 12:00:00", "day")



#data <- as.data.table(data)
#data <- data[TAG==tag &DATE < period_end & DATE > period_start]
#print(data[c("TAG")])

#print(data[(data['TAG']==tag)])
# & (data['DATE'] < period_end)
# data <- data[TAG==tag & DATE < period_end & DATE > period_start]
#print(data[TAG==tag])
