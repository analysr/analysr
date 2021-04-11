impute <-
  function(data,
           tag_wanted,
           period_start,
           period_end,
           temporal_granularity,
           stat_unit_wanted = NULL,
           aggregation_method = mean_aggregate,
           impute_method = linear_impute,
           information_lost_after = 5 * temporal_granularity) {

#let's only take the data we need
    data <- subset (data, tag = tag_wanted)
    data <- subset (data, date > period_start)
    data <- subset (data, date < period_end)
    data <- subset (data, stat_unit == stat_unit_wanted)#ok


# let's initialize our dataframe
    final_date <- seq_date(period_start, period_end, temporal_granularity)
    n <- length(final_date)
    final_tag <- rep(tag_wanted, n)
    final_stat_unit <- rep(stat_unit_wanted, n)
    final_value <- 1:n
    status <- rep("NOT TREATED", n)

    result <- data.frame(final_stat_unit, final_date,
                         final_tag, final_value, status)#ok
    print(n)

# we now have to fill the value column

    #let's start by aggregating the values we do have
    for (i in 1:n){ # i is the index in result

      #in sample are all the values taken in the interval of time that we're
      #working on (for example a day)(between the i date and the (i+1))

      sample <- subset(data, lubridate::ymd_hm(date) >= result$final_date[i])
      print(result$final_date[i])
      sample <- subset(
        data, date < (result$final_date[i] + temporal_granularity))
      print(i)
      print(sample)

      if (length(sample$date) > 0){
        #result$final_value[i] <- aggregate(sample,
                                     #result$final_date[i],
                                     #result$final_date[i] +
                                      # temporal_granularity,
                                    # aggregation_method)
        result$final_value[i] <- aggregation_method(sample$value)
        result$status[i] <- "AGGREGATED"
      }
    }

    #let's complete by imputing the missing values
    for (i in 1:n){

      if ( result$status[i] == "NOT TREATED" ) {
        result$status[i] <- "IMPUTED"
        j <- 1
        while ( i + j < n && result$status[i+j] == "NOT TREATED" ) {
          j <- j + 1
        }
        if (j * temporal_granularity <= information_lost_after){
          impute_method(result, i-1, i+j-1)
        }
      }


    }
    result

    }
