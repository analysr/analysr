test_that("seq_date works", {
  begin <- "1900 01 01"
  end <- "1900 03 24"
  frequency <- 12*lubridate::days()

  excepted_days <- lubridate::as_date(c("1900 01 01", "1900 01 13", "1900 01 25",
                                        "1900 02 06", "1900 02 18", "1900 03 02",
                                        "1900 03 14"))
  result_days <- seq_date2(begin, end, frequency)
  expect_equal(result_days, excepted_days)
})
# CTRL + SHIFT + T
