test_that("fix_granularity works", {

  setup_new_env()
  import_measures_csv(csv_path =
                        "tests/testthat/csv/fix_granularity_csv/
                      test_fix_granularity_before1.csv")

  result <- fix_granularity(tag_wanted = "Kaliemie",
                   period_start = lubridate::ymd_hm("06-11-10 10:00"),
                   period_end = lubridate::ymd_hm("06-11-17 10:00"),
                   temporal_granularity = lubridate::days(),
                   stat_unit_wanted = 101929076)

  stat_unit <- rep(101929076, 8)
  date <- lubridate::ymd_hm("06-11-10 10:00") + lubridate::days() * c(0:7)
  tag <- rep("Kaliemie", 8)
  value <- c(4.7, 7.1, 6.2, 5.3, 6.275, 7.250, 8.225, 6.6)
  status <- c("AGGREGATED", "AGGREGATED", "IMPUTED", "AGGREGATED", "IMPUTED",
              "IMPUTED", "AGGREGATED", "AGGREGATED" )
  hash <- c(1,2,3,4,5,6,7,8)

  expected_result <- data.frame(hash, stat_unit, date, tag, value, status)


  expect_equal(result, expected_result)


  ### DEUXIEME TEST

  setup_new_env()

  expected_result2 <- import_modified_measures_csv(csv_path = "tests/testthat/csv/fix_granularity_csv/test_temperature_after.csv")

  setup_new_env()
  import_measures_csv(csv_path = "tests/testthat/csv/fix_granularity_csv/test_temperature_before.csv")

  result2 <- fix_granularity(
    tag_wanted = "Temperature",
    period_start = lubridate::ymd_hms("10-03-11 10:00:00"),
    period_end = lubridate::ymd_hms("10-03-12 10:00:00"),
    stat_unit_wanted = 108)

  expect_equal(result2, expected_result2)





})
