test_that("induce_period works", {
  setup_new_env()

  expected_measures <- quiet_read_csv(
      file = "./csv/induce_events/after-measures.csv",
      col_types = readr::cols("hash" = "i")
  )$result
  expected_events <- quiet_read_csv(
      file = "./csv/induce_events/after-events.csv",
      col_types = readr::cols("hash" = "i")
  )$result
  expected_stat_units <- quiet_read_csv(
    file = "./csv/induce_events/after-stat_units.csv",
    col_types = readr::cols("hash" = "i")
  )$result

  # import measures
  import_measures_csv("./csv/induce_events/before-measures.csv")

  # induce period
  induce_events(Temperature > 37.5, "Hospitalisation")

  # check values
  expect_equal(dplyr::all_equal(analysr_env$measures, expected_measures), TRUE)
  expect_equal(dplyr::all_equal(analysr_env$events, expected_events), TRUE)
  expect_equal(dplyr::all_equal(analysr_env$stat_units, expected_stat_units),
               TRUE)

  # check that tables are consistent
  expect_equal(check_tables_integrity(), TRUE)

  #check that other tables are empty
  expect_equal(nrow(analysr_env$descriptions), 0)
  expect_equal(nrow(analysr_env$periods), 0)


})
