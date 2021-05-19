
quiet_read_csv <- purrr::quietly(readr::read_csv)

test_that("induce_period works", {
  setup_new_env()

  expected_measures <- as.data.frame(quiet_read_csv(
      file = "./csv/induce_period/after-measures.csv",
      col_types = readr::cols("hash" = "i")
  )$result)
  expected_periods <- as.data.frame(quiet_read_csv(
      file = "./csv/induce_period/after-periods.csv",
      col_types = readr::cols("hash" = "i")
  )$result)

  # import measures
  import_measures_csv("./csv/induce_period/before-measures.csv")

  # induce period
  induce_period(Temperature > 37.5, "Fever", 1*lubridate::days())

  #print(expected_periods)
  #print(analysr_env$periods)

  # check values
  expect_equal(dplyr::all_equal(analysr_env$measures,expected_measures), TRUE)
  #expect_equal(dplyr::all_equal(analysr_env$periods,expected_periods), TRUE)

  # check that tables are consistent
  expect_equal(check_tables_integrity(), TRUE)
})
