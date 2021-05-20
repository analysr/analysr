#' fix_granularity
#'
#' Sets a given granularity to a set of data by aggregating or imputing them.
#'
#' @return The part of analysr_env$measures that has been modified.
#'
#' @param tag_wanted A string containing the tag of the wanted data.
#' @param period_start A date marking the beginning of the studied time period.
#' @param period_end A date marking the end of the studied time period.
#' @param temporal_granularity A duration fixing the wanted granularity.
#' @param stat_unit_wanted An vector of integers containing the stat_units
#' of the wanted data.
#' Default: NULL (means every stat_unit)
#' @param aggregation_method A function to aggregate data.
#' Default: mean_aggregate
#' @param impute_method A function to impute data.
#' Default: linear_impute
#' @param information_lost_after A duration after which, if no data are found,
#' no more will be imputed.
#' Default: 5 * temporal_granularity
#' @export


fix_granularity <-
  function (tag_wanted,
            period_start,
            period_end,
            temporal_granularity,
            stat_unit_wanted = NULL,
            aggregation_method = mean_aggregate,
            impute_method = linear_impute,
            information_lost_after = 5 * temporal_granularity) {

    if (! is.null(stat_unit_wanted)) {
      for (stat in stat_unit_wanted) {
      fix_granularity_aux(tag_wanted,
                          period_start,
                          period_end,
                          temporal_granularity,
                          stat,
                          aggregation_method,
                          impute_method,
                          information_lost_after)
      }
    }
    else {
      stat <- unique(analysr_env$measures$stat_unit)
      fix_granularity(tag_wanted,
                      period_start,
                      period_end,
                      temporal_granularity,
                      stat,
                      aggregation_method,
                      impute_method,
                      information_lost_after)
    }
  }


fix_granularity_aux <-
  function(tag_wanted,
           period_start,
           period_end,
           temporal_granularity,
           stat_unit_wanted = NULL,
           aggregation_method = mean_aggregate,
           impute_method = linear_impute,
           information_lost_after = 5 * temporal_granularity) {

    splits <- split(analysr_env$measures,
                    analysr_env$measures$tag == tag_wanted
                    & analysr_env$measures$date >= period_start
                    & analysr_env$measures$date < period_end + temporal_granularity
                    & analysr_env$measures$stat_unit == stat_unit_wanted)

    data <- splits$"TRUE"
    data_unchanged <- splits$"FALSE"

# let's initialize our dataframe
    date <- seq_date(period_start, period_end, temporal_granularity)
    n <- length(date)
    hash <- get_hash(n)
    tag <- rep(tag_wanted, n)
    stat_unit <- rep(stat_unit_wanted, n)
    value <- 1:n
    status <- rep(NA, n)

    result <- data.frame(hash, stat_unit, date,
                         tag, value, status)

# we now have to fill the value column

    #let's start by aggregating the values we do have
    for (i in 1:n) { # i is the index in result

      #in sample are all the values taken in the interval of time that we're
      #working on (for example a day)(between the i date and the (i+1))

      sample <- subset(data, date >= result$date[i])

      sample <- subset(
        sample, date < (result$date[i] + temporal_granularity))


      if (length(sample$date) > 1) {
        result$value[i] <- aggregation_method(sample$value)
        result$status[i] <- "AGGREGATED"
      }
      else if (length(sample$date) > 0){
        result$value[i] <- sample$value
        result$status[i] <- NA
      }
      else{
        result$status[i] <- "TO BE IMPUTED"
      }
    }

    #let's complete by imputing the missing values
    i <- 1
    while (i < n) {

      if (!is.na(result$status[i]) && result$status[i] == "TO BE IMPUTED" ) {

        result$status[i] <- "IMPUTED"
        j <- 1

        while (i + j < n && !is.na(result$status[i + j]) &&
               result$status[i + j] == "TO BE IMPUTED" ) {

          j <- j + 1
        }

        if (j * temporal_granularity <= information_lost_after) {
          result[(i - 1):(i + j), ] <- impute_method(result, i - 1, i + j)

        }
        else {

          if (i == 1) {
            result <- result[(i+j):n,]
          }

          else if ( i+j == n ) {
            result <- result[1:(i-1),]
          }

          else {
            result <- rbind(result[1:(i-1),], result[(i+j):n,])
          }

          n <- n - j
        }
      }
      i <- i + 1
    }

    analysr_env$measures <- rbind(data_unchanged, result)

    rownames(analysr_env$measures) <- c(1:length(analysr_env$measures$date))

}
