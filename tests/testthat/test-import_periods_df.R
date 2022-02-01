test_that("import periods data frame works", {
  # reset env
  setup_new_env()

  quiet_read_csv <- purrr::quietly(readr::read_delim)

  result <- quiet_read_csv(file = "./csv/import_periods_csv/to_import_1.csv",
                               delim = ",")$result

  import_periods_df(
    result,
    "PERSON",
    "BEGIN",
    "END",
    "DESCRIPTION"
  )

  expect_equal(model_state_equal("./csv/import_periods_csv/after1"), TRUE)
})


