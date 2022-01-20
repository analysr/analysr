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
#' @param date_format_func A function to format date with (not required).
#' Default: `lubridate::parse_date_time(x, date_format_reg)`
#' If you want to use milliseconds [look at this](https://bit.ly/33JGr6s).
#' @param date_format_reg A expression to format date with (not required).
#' Default: `"ymd-HMS"`
#' For more details see [this documentation](https://bit.ly/3bp3FD0).
#' @param force_date_format Boolean to force date format func (not required).
#' Default: `FALSE`
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

    setwd(folder_path)
    l <- list.files(pattern = ".json")

    for (i in l){
      t <- strsplit(i,split="_")
      name <- paste(t[[1]][[1]],t[[1]][[2]])

      result <- fromJSON(i)
      data_raw <- enframe(unlist(result))
      result <- data_raw %>% separate(name, into = c(paste0("x", 1:10)),
                                      fill = "right")
      n <- nrow(data_raw) # get row number
      cev=0
      cme=0
      cpe=0
      events <- data.frame(stat_unit=NULL,date=NULL,tag=NULL)
      periods <- data.frame(stat_unit=NULL,begin=NULL,end=NULL,tag=NULL)
      measures <- data.frame(stat_unit=NULL,date=NULL,tag=NULL,value=NULL)
      for (j in 1:n){
        for (h in 1:10){
          for (t in tag){
            if(!is.NA(match(t,result[[h]][[j]]))){
              r=result[[h]][[j]]
              v=result[[11]][[j]]
              p=TRUE
              a=TRUE
              ap=TRUE
              da=TRUE
              ev=TRUE
              me=TRUE
              jp=j
              while(jp<n&(ap&me)){
                jp=jp+1
                if(result[[h]][[jp]]==r){
                  hp=h
                  while((hp<10)&(is.na(result[[hp+1]][[jp]]))){
                    hp=hp+1
                    if(p){
                      for (b in begin){
                        if(result[[hp]][[jp]]==b){
                          d=result[[11]][[jp]]
                          p=FALSE
                          da=FALSE
                        }
                      }
                    }
                    if(a){
                      for(e in end){
                        if(result[[hp]][[jp]]==e){
                          c=result[[11]][[jp]]
                          a=FALSE
                          ap=FALSE
                          da=FALSE
                          }
                        }
                    }
                    if(da){
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
                    if(!ev){
                      for (mea in value){
                        if(result[[hp]][[jp]]==mea){
                          meas=result[[11]][[jp]]
                          me=FALSE
                          ev=TRUE
                  }
                }
              }
              if(!a&!p&!ap){
                periods <- rbind(periods,c(name,d,c,v))
                cpe=cpe+1
              }
              if(!da&!ev){
                events <- rbind(events,c(name,edate,v))
                cev=cve+1
              }
              if(!me){
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
      )

      analysr_env$periods <- rbind(analysr_env$periods, periods)

      hash <- get_hash(cme)

      result <- cbind(
        hash,
        measures
      )

      analysr_env$measures <- rbind(analysr_env$measures, measures)

      hash <- get_hash(cev)

      result <- cbind(
        hash,
        events
      )

      analysr_env$events <- rbind(analysr_env$events, events)
      }
    }
  }

