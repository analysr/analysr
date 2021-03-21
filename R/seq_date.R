seq_date <- function (begin, end, freq) {
  begin <- lubridate::as_date(begin)
  end <- lubridate::as_date(end)

  i <- lubridate::interval(begin, end)

  nb_pts <- floor(i / freq)
  begin + 0:nb_pts * freq
}
