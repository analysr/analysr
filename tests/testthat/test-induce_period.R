
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
  expected_stat_units <- as.data.frame(quiet_read_csv(
    file = "./csv/induce_period/after-stat_units.csv",
    col_types = readr::cols("hash" = "i")
  )$result)

  # import measures
  import_measures_csv("./csv/induce_period/before-measures.csv")

  # induce period
  induce_period(Temperature > 37.0, "Fever", 1*days)
  induce_period(Temperature, "Fever", 1*days)

  # Pour l'instalation de stringr:
  #Install the released version from CRAN:
  #install.packages("stringr")
  # Install the cutting edge development version from GitHub:
  # install.packages("devtools")
  #devtools::install_github("tidyverse/stringr")


  # check values
  expect_equal(dplyr::all_equal(analysr_env$measures, expected_measures), TRUE)
  expect_equal(dplyr::all_equal(analysr_env$periods, expected_periods), TRUE)
  expect_equal(dplyr::all_equal(analysr_env$stat_units, expected_stat_units),
               TRUE)

  # check that tables are consistent
  expect_equal(check_tables_integrity(), TRUE)

  #check that other tables are empty
  expect_equal(nrow(analysr_env$descriptions), 0)
  expect_equal(nrow(analysr_env$events), 0)


})
