test_that("fix_granularity works", {

  ### FIRST TEST ###

  setup_new_env()

  expected_result <- import_measures_csv(
    csv_path = "./csv/fix_granularity_csv/test_fix_granularity_after1.csv",
    date_format_reg = "ymd-HM")

  # using readr may be better

  setup_new_env()
  import_measures_csv(
    csv_path = "./csv/fix_granularity_csv/test_fix_granularity_before1.csv",
    date_format_reg = "ymd-HM")


  result <- fix_granularity(
    tag_wanted = "Kaliemie",
    period_start = lubridate::ymd_hm("06-11-10 10:00"),
    period_end = lubridate::ymd_hm("06-11-17 10:00"),
    stat_unit_wanted = 101929076,
    temporal_granularity = lubridate::days())

  print(result)
  print(expected_result)

  expect_equal(result[c("stat_unit", "date", "tag", "value", "status")],
               expected_result[c("stat_unit", "date", "tag", "value", "status")])


  ### SECOND TEST ###

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
