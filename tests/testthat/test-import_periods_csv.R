# to compare dataframes : https://bit.ly/3gNYsZ4

quiet_read_csv <- purrr::quietly(readr::read_csv)

test_that("import periods CSV  works", {
  # reset env
  setup_new_env()

  import_periods_csv("./csv/import_periods_csv/to_import_1.csv",
                     "PERSON",
                     "BEGIN",
                     "END",
                     "DESCRIPTION")
  expect_equal(model_state_equal("./csv/import_periods_csv/after1"), TRUE)
})

test_that("import periods CSV works when import twice", {
  # reset env
  setup_new_env()

  # import twice
  import_periods_csv("./csv/import_periods_csv/to_import_2.csv",
                     "PERSON",
                     "BEGIN",
                     "END",
                     "DESCRIPTION")
  import_periods_csv("./csv/import_periods_csv/to_import_2.csv",
                     "PERSON",
                     "BEGIN",
                     "END",
                     "DESCRIPTION")
  expect_equal(model_state_equal("./csv/import_periods_csv/after2"), TRUE)
})

test_that("import periods CSV works and fill descriptions", {
  # reset env
  setup_new_env()

  # import
  import_periods_csv("./csv/import_periods_csv/to_import_1.csv",
                     "PERSON",
                     "BEGIN",
                     "END",
                     "DESCRIPTION",
                     c("LOCATION"))
  expect_equal(model_state_equal("./csv/import_periods_csv/after3"), TRUE)
})
test_that("import periods CSV works when importing different date formats", {

  # expected
  expected <- as.data.frame(quiet_read_csv(
      file = "./csv/import_periods_csv/date/after.csv",
      col_types = readr::cols("hash" = "i")
  )$result)

  # import ymd-HM
  setup_new_env()
  import_periods_csv(
    "./csv/import_periods_csv/date/before-ymd-HM.csv",
    date_format_reg = "ymd-HM"
  )

  expect_equal(
    dplyr::all_equal(expected,
      analysr_env$periods), TRUE)

  # import dmy-HMS
  setup_new_env()
  import_periods_csv(
    "./csv/import_periods_csv/date/before-dmy-HMS.csv",
    date_format_reg = "dmy-HMS"
  )

  expect_equal(
    dplyr::all_equal(expected,
      analysr_env$periods), TRUE)
})
