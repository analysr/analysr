test_that("setup_new_env works", {

  setup_new_env()

  # test on measures table

  expected_measures <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(expected_measures) <-
    c("hash", "stat_unit", "date", "tag", "value")

  expect_equal(
    dplyr::all_equal(
      analysr_env$measures,
      expected_measures), TRUE)

  # test on periods table

  expected_periods <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(expected_periods) <-
    c("hash", "stat_unit", "begin", "end", "desc")

  expect_equal(
    dplyr::all_equal(
      analysr_env$periods,
      expected_periods), TRUE)

  # test on events table

  expected_events <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(expected_events) <- c("hash", "stat_unit", "date", "tag")

  expect_equal(
    dplyr::all_equal(
      analysr_env$events,
      expected_events), TRUE)

  # test on stat_units table

  expected_stat_units <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(expected_stat_units) <- c("hash", "stat_unit")

  expect_equal(
    dplyr::all_equal(
      analysr_env$stat_units,
      expected_stat_units), TRUE)

  # test on descriptions table

  expected_descriptions <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(expected_descriptions) <- c("hash", "type", "value")

  expect_equal(
    dplyr::all_equal(
      analysr_env$descriptions,
      expected_descriptions), TRUE)

  expect_equal(
    analysr_env$current_hash,1
  )

})
