test_that("add_stat_units works", {
  setup_new_env()

  # add some elements
  add_stat_units(c(144545, 78787, 845, 144545))

  expected <- data.frame(hash = c(1L, 2L, 3L),
                         stat_unit = c(144545, 78787, 845))

  expect_equal(dplyr::all_equal(expected, analysr_env$stat_units), TRUE)

  # try to add a element
  add_stat_units(c(845))

  expect_equal(dplyr::all_equal(expected, analysr_env$stat_units), TRUE)

})
