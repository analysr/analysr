linear_impute <-

  function(value_start,
           value_end,
           data){

    #print("ok")
    n <- length(data$stat_unit)

    data$value[1] <- value_start

    #print(data, "ok")

    a <- as.double(data$date[2] - data$date[1])
    b <- as.double(data$date[n] - data$date[1])
    c <- a/b

    #print("c",c)

    imputed_values <- value_start + c * (value_end - value_start) * c(0:(n-1))
    #print(imputed_values)

    data$value <- imputed_values

    #data$value[n] <- value_end

    data
  }
