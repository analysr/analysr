aggregate <-
  function (data,
            interval_start,
            interval_end,
            method){

    data <- subset(data, date < interval_end)
    data <- subset(data, date > interval_start)
    print (data)
    method(data$value)


  }
