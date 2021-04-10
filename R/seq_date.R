# voir le package lubridate en général
# install.packages("lubridate")

# seq_date("2020/12/17", "2020/12/24", lubridate::days())

seq_date <- function (begin, end, freq) {
  begin <- lubridate::as_date(begin)
  end <- lubridate::as_date(end)

  i <- lubridate::interval(begin, end)

  result <- begin
  while (begin + freq < end) {
    begin <- begin + freq
    result <- c(result, begin)
  }
  result
}


seq_date2 <- function (begin, end, freq2) {
  begin <- lubridate::as_date(begin)
  end <- lubridate::as_date(end)

  i <- lubridate::interval(begin, end)

  nb_pts <- floor(i / freq2)
  begin + 0:nb_pts * freq2
}
