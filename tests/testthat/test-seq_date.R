test_that("seq_date works", {
  begin <- lubridate::ymd_hm("00 03 01 00 00")
  end <- lubridate::ymd_hm("00 05 15 00 00")
  frequency <- 12*lubridate::days()

  excepted_days <- lubridate::ymd_hm(c("00 03 01 00 00", "00 03 13 00 00",
                                        "00 03 25 00 00", "00 04 06 00 00",
                                        "00 04 18 00 00", "00 04 30 00 00",
                                        "00 05 12 00 00"))
  result_days <- seq_date(begin, end, frequency)
  expect_equal(result_days, excepted_days)
})
# CTRL + SHIFT + T
