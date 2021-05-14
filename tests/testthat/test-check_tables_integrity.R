test_that("check_tables_integrity works", {
  setup_new_env()

  # without changing anything
  expect_equal(check_tables_integrity(), TRUE)

  # use quietly
  quiet_check <- purrr::quietly(check_tables_integrity)

  # bad order on periods table
  setup_new_env()
  analysr_env$periods <- data.frame(stat_unit = integer(0), hash = integer(0),
                      begin = numeric(0), end = numeric(0), desc = character(0))
  expect_equal(quiet_check()$result, FALSE)


  # bad labels on measures table
  setup_new_env()
  analysr_env$measures <- data.frame(hash = integer(0), stat_unit = integer(0),
                      begin = numeric(0), end = numeric(0), desc = character(0))
  expect_equal(quiet_check()$result, FALSE)

  # misspelling in descriptions table
  setup_new_env()
  analysr_env$descriptions <- data.frame(hash = integer(0), type = character(0),
                                        values = character(0))
  expect_equal(quiet_check()$result, FALSE)

  # bad order on stat_units table
  setup_new_env()
  analysr_env$descriptions <- data.frame(stat_unit = integer(0),
                                        hash = integer(0))
  expect_equal(quiet_check()$result, FALSE)
})
