linear_impute <-

  function(data, first_line, last_line){
    data <- data[first_line:last_line,]
# here the first and last values of data are correct and we want to impute the
    # in-between values

    n <- length(data$stat_unit)

    value_start <- data$value[1]
    value_end <- data$value[n]

    data$value[1] <- value_start

    a <- as.double(data$date[2] - data$date[1])
    b <- as.double(data$date[n] - data$date[1])
    c <- a/b

    imputed_values <- value_start + c * (value_end - value_start) * c(0:(n-1))

    data$value <- imputed_values

    data$value[n] <- value_end

    data
  }
