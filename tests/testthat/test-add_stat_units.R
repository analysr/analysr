test_that("add_stat_units works", {
  setup_new_env()

  # add some elements
  add_stat_units(c(144545, 78787, 845))

  # Check that stat_units number is 3
  expect_equal(nrow(analysr_env$stat_units), 3)

  # try to add a element
  add_stat_units(c(144545))

  # Check that stat_units number is still 3
  expect_equal(nrow(analysr_env$stat_units), 3)
})
