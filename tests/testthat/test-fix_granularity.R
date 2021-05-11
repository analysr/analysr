test_that("fix_granularity works", {




  ### DEUXIEME TEST

  setup_new_env()

  expected_result2 <- import_modified_measures_csv(
    csv_path = "./csv/fix_granularity_csv/test_temperature_after.csv")

  setup_new_env()
  import_measures_csv(
    csv_path = "./csv/fix_granularity_csv/test_temperature_before.csv")


  result2 <- fix_granularity(
    tag_wanted = "Temperature",
    period_start = lubridate::ymd_hms("10-03-11 10:00:00"),
    period_end = lubridate::ymd_hms("10-03-12 10:00:00"),
    stat_unit_wanted = 108,
    temporal_granularity = lubridate::hours())

  print(result2)
  print(expected_result2)

  expect_equal(result2, expected_result2)





})
