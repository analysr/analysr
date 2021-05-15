test_that("linear_impute works", {

  stat_unit <- c(1453, 1453, 1453, 1453, 1453, 1453, 1453, 1453, 1453)
  date <- lubridate::as_date(c("1900 01 01", "1900 01 02", "1900 01 03",
                               "1900 01 04", "1900 01 05", "1900 01 06",
                               "1900 01 07", "1900 01 08", "1900 01 09"))
  value <- c(1:9)
  value[1] <- 3.0
  value[9] <- 9.6
  tag <- c("kaliemie", "kaliemie", "kaliemie", "kaliemie", "kaliemie",
           "kaliemie", "kaliemie", "kaliemie", "kaliemie")
  data <- data.frame(stat_unit, date, tag, value)

  result <- linear_impute(data, 1, 9)

  value <- c(3.0, 3.825, 4.65, 5.475, 6.3, 7.125, 7.95, 8.775, 9.6)
  expected_result <- data.frame(stat_unit, date, tag, value)

  expect_equal(result, expected_result)
})
