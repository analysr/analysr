linear_impute <-

  function(data, first_line, last_line){
    data <- data[first_line:last_line,]
# here the first and last values of data are correct and we want to impute the
    # in-between values

    n <- length(data$stat_unit)


    value_start <- data$value[1]
    value_end <- data$value[n]

    data$value[1] <- value_start

    d_time <- as.double(data$date[2] - data$date[1])
    global_time_difference <- as.double(data$date[n] - data$date[1])
    global_value_difference <- value_end - value_start
    d_value <- global_value_difference * d_time / global_time_difference


    imputed_values <- value_start + d_value * c(0:(n-1))

    data$value <- imputed_values #problème !

    data$value[n] <- value_end

    data
  }
