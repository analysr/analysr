test_that("import measures data frame  works", {
  # reset env
  setup_new_env()

  quiet_read_csv <- purrr::quietly(readr::read_delim)

  result <- quiet_read_csv(file = "./csv/import_measures_csv/to_import_1.csv",
                               delim = ",")$result

  import_measures_df(
    result,
    "patient",
    "date_prlvt",
    "type_examen",
    "valeur"
  )
  expect_equal(model_state_equal("./csv/import_measures_csv/after1"), TRUE)
})
