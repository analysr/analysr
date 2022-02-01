test_that("import events data frame  works", {
  # reset env
  setup_new_env()

  quiet_read_csv <- purrr::quietly(readr::read_delim)

  result <- quiet_read_csv(file = "./csv/import_events_csv/to_import_1.csv",
                               delim = ",")$result

  import_events_df(result,
                    "PERSON",
                    "TIMESTAMP",
                    "TITLE")

  expect_equal(model_state_equal("./csv/import_events_csv/after1"), TRUE)

})
