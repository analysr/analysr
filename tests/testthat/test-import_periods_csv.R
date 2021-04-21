# to compare dataframes :
# https://community.rstudio.com/t/all-equal-on-tibbles-ignores-attributes/4299/2

test_that("import periods CSV  works", {
  # reset env
  setup_new_env()

  import_periods_csv("./csv/import_periods_csv/before-advanced.csv",
                     "PERSON",
                     "BEGIN",
                     "END",
                     "DESCRIPTION")

  quiet_read_csv <- purrr::quietly(readr::read_csv)
  excepted <-
    as.data.frame(quiet_read_csv(
      file = "./csv/import_periods_csv/after.csv")$result
    )

  # to check dataframes without hash
  expect_equal(
    dplyr::all_equal(
      analysr_env$periods[c("stat_unit", "begin", "end", "desc")],
      excepted), TRUE)


  # check that stat units have been added
  expect_equal(nrow(analysr_env$stat_units), 2)

  # check if hash column exist in dataframe
  expect_equal("hash" %in% colnames(analysr_env$periods), TRUE)

  # check if hash is first column
  expect_equal("hash", colnames(analysr_env$periods)[1])

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

  quiet_read_csv <- purrr::quietly(readr::read_csv)

  excepted <-
    as.data.frame(quiet_read_csv(
      file = "./csv/import_periods_csv/after2.csv"
    )$result)

  # to check dataframes without hash
  expect_equal(
    dplyr::all_equal(excepted,
      analysr_env$periods[c("stat_unit", "begin", "end", "desc")]), TRUE)
  # check that stat units have been added
  expect_equal(nrow(analysr_env$stat_units), 2)

  # check if current hash has changed in env
  expect_equal(analysr_env$current_hash, 7)

  # check if hash column exist in dataframe colnames
  expect_equal("hash" %in% colnames(analysr_env$periods), TRUE)

  # check if hash is first column
  expect_equal("hash", colnames(analysr_env$periods)[1])

})
