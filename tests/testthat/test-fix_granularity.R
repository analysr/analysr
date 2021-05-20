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


  fix_granularity(
    tag_wanted = "Kaliemie",
    period_start = lubridate::ymd("06-11-10"),
    period_end = lubridate::ymd("06-11-17"),
    stat_unit_wanted = c(101929076, 101929077),
    temporal_granularity = lubridate::days())

  expect_equal(analysr_env$measures[c("stat_unit", "date", "tag", "value", "status")],
               expected_result[c("stat_unit", "date", "tag", "value", "status")])


  ### SECOND TEST ###

  setup_new_env()

  expected_result2 <- import_measures_csv(
    csv_path = "./csv/fix_granularity_csv/test_temperature_after.csv")

  # using readr may be better

  setup_new_env()
  import_measures_csv(
    csv_path = "./csv/fix_granularity_csv/test_temperature_before.csv")


  fix_granularity(
    tag_wanted = "Temperature",
    period_start = lubridate::ymd_hms("10-03-11 10:00:00"),
    period_end = lubridate::ymd_hms("10-03-12 10:00:00"),
    stat_unit_wanted = c(108),
    temporal_granularity = lubridate::hours())

  expect_equal(analysr_env$measures[c("stat_unit", "date", "tag", "value", "status")],
      expected_result2[c("stat_unit", "date", "tag", "value", "status")])



  ### THIRD TEST ###
  ## Test if the gaps are well treated
  setup_new_env()

  expected_result3 <- import_measures_csv(
      csv_path = "./csv/fix_granularity_csv/test_temperature_gap_after.csv")

  # using readr may be better

  setup_new_env()
  import_measures_csv(
    csv_path = "./csv/fix_granularity_csv/test_temperature_gap_before.csv")


  fix_granularity(
    tag_wanted = "Temperature",
    period_start = lubridate::ymd_hms("10-03-11 10:00:00"),
    period_end = lubridate::ymd_hms("10-03-12 10:00:00"),
    stat_unit_wanted = c(108),
    temporal_granularity = lubridate::hours())

  expect_equal(analysr_env$measures[c("stat_unit", "date", "tag", "value", "status")],
               expected_result3[c("stat_unit", "date", "tag", "value", "status")])


  ### FOURTH TEST ###
  ## Test if the gaps are well treated (one gap in the middle and one at the end)
  setup_new_env()

  expected_result4 <- import_measures_csv(
    csv_path = "./csv/fix_granularity_csv/test_temperature_gap_after.csv")

  # using readr may be better

  setup_new_env()
  import_measures_csv(
    csv_path = "./csv/fix_granularity_csv/test_temperature_gap_before.csv")


  fix_granularity(
    tag_wanted = "Temperature",
    period_start = lubridate::ymd_hms("10-03-11 10:00:00"),
    period_end = lubridate::ymd_hms("10-03-13 10:00:00"),
    stat_unit_wanted = c(108),
    temporal_granularity = lubridate::hours())
  expect_equal(analysr_env$measures[c("stat_unit", "date", "tag", "value", "status")],
               expected_result4[c("stat_unit", "date", "tag", "value", "status")])

  ### FIFTH TEST ###
  ## Test if the gaps are well treated (one gap in the middle and one at the beginning)
  setup_new_env()

  expected_result5 <- import_measures_csv(
    csv_path = "./csv/fix_granularity_csv/test_temperature_gap_after.csv")

  setup_new_env()
  import_measures_csv(
    csv_path = "./csv/fix_granularity_csv/test_temperature_gap_before.csv")


  fix_granularity(
    tag_wanted = "Temperature",
    period_start = lubridate::ymd_hms("10-03-10 10:00:00"),
    period_end = lubridate::ymd_hms("10-03-12 10:00:00"),
    stat_unit_wanted = c(108),
    temporal_granularity = lubridate::hours())

  expect_equal(analysr_env$measures[c("stat_unit", "date", "tag", "value", "status")],
               expected_result5[c("stat_unit", "date", "tag", "value", "status")])
})
