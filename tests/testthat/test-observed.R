test_that("observed works with simple condition", {
  setup_new_env()

  # import measures
  load_env_csv("./csv/observed/before1")

  # observed
  model <- observed(analysr_env, Temperature > 38.5)
  query <- list(tag="Temperature")

  # check model
  expect_equal(model_state_equal("./csv/observed/after1", model, query), TRUE)
})
test_that("observed works  with simple reverse condition", {
  setup_new_env()

  # import measures
  load_env_csv("./csv/observed/before1")

  # observed
  model <- observed(analysr_env, 38.5 < Temperature)
  query <- list(tag="Temperature")

  # check model
  expect_equal(model_state_equal("./csv/observed/after1", model, query), TRUE)
})
