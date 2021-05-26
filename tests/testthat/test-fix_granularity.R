test_that("fix_granularity works (test 1)", {

  setup_new_env()

  load_env_csv("./csv/fix_granularity_csv/before1")

  fix_granularity(
    tag_wanted = "Kaliemie",
    period_start = lubridate::ymd("06-11-10"),
    period_end = lubridate::ymd("06-11-17"),
    stat_unit_wanted = c(101929076, 101929077),
    temporal_granularity = lubridate::days())

  expect_equal(model_state_equal("./csv/fix_granularity_csv/after1"), TRUE)

})
test_that("fix_granularity works (test 2)", {
  setup_new_env()

  load_env_csv("./csv/fix_granularity_csv/before2")

  fix_granularity(
    tag_wanted = "Temperature",
    period_start = lubridate::ymd_hms("10-03-11 10:00:00"),
    period_end = lubridate::ymd_hms("10-03-12 10:00:00"),
    stat_unit_wanted = c(108),
    temporal_granularity = lubridate::hours())

  expect_equal(model_state_equal("./csv/fix_granularity_csv/after2"), TRUE)
})
test_that("fix_granularity works: test if the gaps are well treated", {
  setup_new_env()

  load_env_csv("./csv/fix_granularity_csv/before3")

  fix_granularity(
    tag_wanted = "Temperature",
    period_start = lubridate::ymd_hms("10-03-11 10:00:00"),
    period_end = lubridate::ymd_hms("10-03-12 10:00:00"),
    stat_unit_wanted = c(108),
    temporal_granularity = lubridate::hours())

  expect_equal(model_state_equal("./csv/fix_granularity_csv/after3"), TRUE)
})
test_that("fix_granularity works: test if the gaps are well treated
          (one gap in the middle and one at the end)", {
  setup_new_env()

  load_env_csv("./csv/fix_granularity_csv/before4")

  fix_granularity(
    tag_wanted = "Temperature",
    period_start = lubridate::ymd_hms("10-03-11 10:00:00"),
    period_end = lubridate::ymd_hms("10-03-13 10:00:00"),
    stat_unit_wanted = c(108),
    temporal_granularity = lubridate::hours())

  expect_equal(model_state_equal("./csv/fix_granularity_csv/after4"), TRUE)
})
test_that("fix_granularity works: test if the gaps are well treated
          (one gap in the middle and one at the beginning)", {
  setup_new_env()

  load_env_csv("./csv/fix_granularity_csv/before5")

  fix_granularity(
    tag_wanted = "Temperature",
    period_start = lubridate::ymd_hms("10-03-10 10:00:00"),
    period_end = lubridate::ymd_hms("10-03-12 10:00:00"),
    stat_unit_wanted = c(108),
    temporal_granularity = lubridate::hours())

  expect_equal(model_state_equal("./csv/fix_granularity_csv/after5"), TRUE)
})
