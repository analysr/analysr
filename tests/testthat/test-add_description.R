test_that("add_description works", {
  setup_new_env()

  # import model
  load_env_csv("./csv/add_description/before")

  add_description(c(1, 6, 6), "Fever")


  # check model
  expect_equal(model_state_equal("./csv/add_description/after1", analysr_env), TRUE)

})
test_that("add_description works with model as input", {
  setup_new_env()

  # import model
  load_env_csv("./csv/add_description/before")

  # generate selection
  date <- lubridate::parse_date_time("2006/11/09 08:00:00", "ymd-HMS")
  analysr_env$selection <- tibble::tibble(hash_stat_unit = c(1, 2),
                                           stat_unit = c(1, 6),
                                           hash_obs = c(3, 4),
                                           date_obs = c(date, date))

  add_description(analysr_env, "Fever")


  # check model
  expect_equal(model_state_equal("./csv/add_description/after1", analysr_env), TRUE)

})
test_that("add_description works with space", {
  setup_new_env()

  # import model
  load_env_csv("./csv/add_description/before")

  add_description(c(2, 7), "Fever after surgery")


  # check model
  expect_equal(model_state_equal("./csv/add_description/after2", analysr_env), TRUE)

})
