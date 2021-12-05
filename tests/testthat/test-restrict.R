test_that("restrict works on stat_units description", {
  setup_new_env()

  # import measures
  load_env_csv("./csv/restrict/before1")
  # restrict
  model <- restrict(analysr_env, Gender == "Female")

  # check model
  expect_equal(model_state_equal("./csv/restrict/after1", model), TRUE)
})
test_that("restrict works on measures description", {
  setup_new_env()

  # import measures
  load_env_csv("./csv/restrict/before2")

  # restrict
  model <- restrict(analysr_env, Team == "Night")

  # check model
  expect_equal(model_state_equal("./csv/restrict/after2", model), TRUE)

  # this test also test that restrict do not duplicate stat_units
})
test_that("restrict works on measures table", {
  setup_new_env()

  # import measures
  load_env_csv("./csv/restrict/before3")

  # restrict
  model <- restrict(analysr_env, Temperature > 38)

  # check model
  expect_equal(model_state_equal("./csv/restrict/after3", model), TRUE)
})
test_that("restrict works on events table", {
  setup_new_env()

  # import measures
  load_env_csv("./csv/restrict/before4")

  # restrict
  model <- restrict(analysr_env, Hemorrhage)

  # check model
  expect_equal(model_state_equal("./csv/restrict/after4", model), TRUE)
})
test_that("restrict works on periods table", {
  setup_new_env()

  # import measures
  load_env_csv("./csv/restrict/before5")

  # restrict
  model <- restrict(analysr_env, Hospitalization)

  # check model
  expect_equal(model_state_equal("./csv/restrict/after5", model), TRUE)
})
