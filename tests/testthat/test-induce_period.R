test_that("induce_period works with simple condition", {
  setup_new_env()

  # import measures
  load_env_csv("./csv/induce_period/before1")

  # induce period
  induce_period(analysr_env, Temperature > 38.5, "Fever", 1 * days)

  # check model
  expect_equal(model_state_equal("./csv/induce_period/after1"), TRUE)
})
test_that("induce_period works with simple reverse condition", {
  setup_new_env()

  # import measures
  load_env_csv("./csv/induce_period/before1")

  # induce period
  induce_period(analysr_env, 38.5 < Temperature, "Fever", 1 * days)

  # check model
  expect_equal(model_state_equal("./csv/induce_period/after1"), TRUE)
})
