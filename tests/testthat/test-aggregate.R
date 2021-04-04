test_that("aggregate works", {

  method <- min_aggregate
  interval_start <- lubridate::ymd_hms(061023000200)
  interval_end <- lubridate::ymd_hms(061023235900)

  data <- as.data.frame(
    readr::read_csv("csv/import_measures_csv/after.csv"))

  result <- aggregate(data, interval_start, interval_end, method)

  expect_equal(result, 0.4)

})
