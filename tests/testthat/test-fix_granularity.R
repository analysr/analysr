test_that("fix_granularity works", {

  setup_new_env()

  expected_result2 <- import_measures_csv(
    csv_path = "./csv/fix_granularity_csv/test_temperature_after.csv")

  # using readr may be better

  setup_new_env()
  import_measures_csv(
    csv_path = "./csv/fix_granularity_csv/test_temperature_before.csv")


  result2 <- fix_granularity(
    tag_wanted = "Temperature",
    period_start = lubridate::ymd_hms("10-03-11 10:00:00"),
    period_end = lubridate::ymd_hms("10-03-12 10:00:00"),
    stat_unit_wanted = 108,
    temporal_granularity = lubridate::hours())

  expect_equal(result2[c("stat_unit", "date", "tag", "value", "status")],
      expected_result2[c("stat_unit", "date", "tag", "value", "status")])


})
