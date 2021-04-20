seq_date <- function(begin, end, freq) {
  begin <- begin
  end <- end

  i <- lubridate::interval(begin, end)

  nb_pts <- floor(i / freq)
  begin + 0:nb_pts * freq
}