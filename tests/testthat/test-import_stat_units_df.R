test_that("import stat_units CSV  works", {
  # reset env
  setup_new_env()

  quiet_read_csv <- purrr::quietly(readr::read_delim)

  result <- quiet_read_csv(file = "./csv/import_stat_units_csv/to_import_1.csv",
                               delim = ",")$result

  import_stat_units_df(
    result,
    "UserId",
    c("BIRTHDATE", "DEATHDATE", "FIRST", "LAST", "GENDER")
  )
  expect_equal(model_state_equal("./csv/import_stat_units_csv/after1"), TRUE)
})
