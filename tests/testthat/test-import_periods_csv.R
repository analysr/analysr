# to compare dataframes : https://bit.ly/3gNYsZ4

quiet_read_csv <- purrr::quietly(readr::read_csv)

test_that("import periods CSV  works", {
  # reset env
  setup_new_env()

  import_periods_csv("./csv/import_periods_csv/before-advanced.csv",
                     "PERSON",
                     "BEGIN",
                     "END",
                     "DESCRIPTION")

  expected <-
    as.data.frame(quiet_read_csv(
      file = "./csv/import_periods_csv/after.csv")$result
    )

  # to check dataframes without hash
  expect_equal(
    dplyr::all_equal(
      analysr_env$periods[c("stat_unit", "begin", "end", "desc")],
      expected), TRUE)


  # check that stat units have been added
  expect_equal(nrow(analysr_env$stat_units), 2)

  # check that tables are consistent
  expect_equal(check_tables_integrity(), TRUE)

  # check if current hash has changed in env
  expect_equal(analysr_env$current_hash, 5)
})

test_that("import periods CSV works when import twice", {
  # reset env
  setup_new_env()

  # import twice
  import_periods_csv("./csv/import_periods_csv/before.csv",
                     "PERSON",
                     "BEGIN",
                     "END",
                     "DESCRIPTION")
  import_periods_csv("./csv/import_periods_csv/before.csv",
                     "PERSON",
                     "BEGIN",
                     "END",
                     "DESCRIPTION")

  expected <-
    as.data.frame(quiet_read_csv(
      file = "./csv/import_periods_csv/after2.csv"
    )$result)

  # to check dataframes without hash
  expect_equal(
    dplyr::all_equal(expected,
      analysr_env$periods[c("stat_unit", "begin", "end", "desc")]), TRUE)
  # check that stat units have been added
  expect_equal(nrow(analysr_env$stat_units), 2)

  # check if current hash has changed in env
  expect_equal(analysr_env$current_hash, 7)

  # check that tables are consistent
  expect_equal(check_tables_integrity(), TRUE)
})

test_that("import periods CSV works and fill descriptions", {
  # reset env
  setup_new_env()

  # import
  import_periods_csv("./csv/import_periods_csv/before-advanced.csv",
                     "PERSON",
                     "BEGIN",
                     "END",
                     "DESCRIPTION",
                     c("LOCATION"))

  expected <-
    as.data.frame(quiet_read_csv(
      file = "./csv/import_periods_csv/after.csv")$result
    )

  # to check dataframes without hash
  expect_equal(
    dplyr::all_equal(
      analysr_env$periods[c("stat_unit", "begin", "end", "desc")],
      expected), TRUE)

  expected_descriptions <-
    as.data.frame(quiet_read_csv(
      file = "./csv/import_periods_csv/after-descriptions.csv")$result
    )
  expected_descriptions <- transform(expected_descriptions,
                                     hash = as.integer(hash))
  # conflict when importing hash have to be an integer

  expect_equal(dplyr::all_equal(
      analysr_env$descriptions, expected_descriptions
  ), TRUE)

  # check that tables are consistent
  expect_equal(check_tables_integrity(), TRUE)
})
test_that("import periods CSV works when importing different date formats", {

  # expected
  expected <- as.data.frame(quiet_read_csv(
      file = "./csv/import_periods_csv/date/after.csv")$result)

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