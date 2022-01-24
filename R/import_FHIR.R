library(jsonlite)
library(tibble)
library(tidyr)

#' import_fhir
#'
#' Import data from a FHIR folder
#'
#' @return Merge of imported data and already imported data
#'
#' @param folder_path A path to the csv file.
#' @param date A vector of labels containing dates.
#' @param begin A vector of labels containing begin dates.
#' @param end A vector of labels containing end dates.
#' @param tag A vector of labels containing tags.
#' @param value A vector of labels containing values.
#' @param optional_data A vector containing label to import in descriptions
#' table.
#'
#' @export
import_fhir <-
  function(folder_path,
           date = c("date"),
           begin = c("begin"),
           end = c("end"),
           tag = c("tag"),
           value = c("value"),
           optional_data){

    setwd(folder_path) # set working directory in the FHIR folder
    l <- list.files(pattern = ".json") # make a list of the names of FHIR files

    for (i in l){
      t <- strsplit(i,split="_")
      name <- paste(t[[1]][[1]],t[[1]][[2]]) # keep the name of the patient

      result <- fromJSON(i) # import data from JSON
      data_raw <- enframe(unlist(result)) # store data in two columns
      result <- data_raw %>% separate(name, into = c(paste0("x", 1:10)),
                                      fill = "right") # store data in 11 columns
      n <- nrow(data_raw) # get row number
      cev=0 # number of events added
      cme=0 # number of measures added
      cpe=0 # number of periods added
      events <- data.frame(stat_unit=NULL,date=NULL,tag=NULL)
      periods <- data.frame(stat_unit=NULL,begin=NULL,end=NULL,tag=NULL)
      measures <- data.frame(stat_unit=NULL,date=NULL,tag=NULL,value=NULL)
      for (j in 1:n){
        for (h in 1:10){
          for (t in tag){
            if(!is.NA(match(t,result[[h]][[j]]))){ # checking if tag is found
              r=result[[h]][[j]] # keeping tag
              v=result[[11]][[j]] # keeping value of tag
              p=TRUE #TRUE if we need to check for a begin date
              a=TRUE #TRUE if we need to check for an end date
              ap=TRUE #FALSE if we have found a period
              da=TRUE #TRUE if we need to check for a date
              ev=TRUE #TRUE if we have found a date
              me=TRUE #TRUE if we have found a measure
              jp=j # store row value
              while(jp<n&(ap&me)){
                jp=jp+1
                if(result[[h]][[jp]]==r){ # looking for the same tag
                  hp=h # store column value
                  while((hp<10)&(is.na(result[[hp+1]][[jp]]))){
                    hp=hp+1
                    if(p){
                      for (b in begin){ # looking for a begin date
                        if(result[[hp]][[jp]]==b){
                          d=result[[11]][[jp]]
                          p=FALSE
                          da=FALSE
                        }
                      }
                    }
                    if(a){ # looking for an end date
                      for(e in end){
                        if(result[[hp]][[jp]]==e){
                          c=result[[11]][[jp]]
                          a=FALSE
                          ap=FALSE
                          da=FALSE
                          }
                        }
                    }
                    if(da){ # looking for a date
                      for (co in date){
                        if(result[[hp]][[jp]]==co){
                          edate=result[[11]][[jp]]
                          da=FALSE
                          ev=FALSE
                          a=FALSE
                          b=FALSE
                        }
                      }
                    }
                    if(!ev|da){ # looking for a measure
                      for (mea in value){
                        if(result[[hp]][[jp]]==mea){
                          meas=result[[11]][[jp]]
                          me=FALSE
                          ev=TRUE
                  }
                }
              }
              if(!a&!p&!ap){ # adding a period
                periods <- rbind(periods,c(name,d,c,v))
                cpe=cpe+1
              }
              if(!da&!ev){ # adding an event
                events <- rbind(events,c(name,edate,v))
                cev=cve+1
              }
              if(!me){ # adding a measure
                measures <- rbind(measures,c(name,edate,v))
                cme=cme+1
              }
            }
          }
        }
      }
    }
        }
      hash <- get_hash(cpe)

      result <- cbind(
        hash,
        periods
      ) # adding the hash to periods

      analysr_env$periods <- rbind(analysr_env$periods, periods)

      hash <- get_hash(cme)

      result <- cbind(
        hash,
        measures
      ) # adding the hash to measures

      analysr_env$measures <- rbind(analysr_env$measures, measures)

      hash <- get_hash(cev)

      result <- cbind(
        hash,
        events
      ) # adding the hash to events

      analysr_env$events <- rbind(analysr_env$events, events)
      }
    }
  }

