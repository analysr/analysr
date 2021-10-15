test_that("observed works with simple condition", {
  setup_new_env()

  # import measures
  load_env_csv("./csv/observed/before1")

  # observed
  model <- observed(analysr_env, Temperature > 38.5)

  query <- list(condition = rlang::expr(Temperature > 38.5),
                tag = "Temperature")

  # check model
  # check here with before (as nothing should have changed on model)
  expect_equal(model_state_equal("./csv/observed/before1", model, query), TRUE)
})
test_that("observed works  with simple reverse condition", {
  setup_new_env()

  # import measures
  load_env_csv("./csv/observed/before1")

  # observed
  model <- observed(analysr_env, 38.5 < Temperature)


  query <- list(condition = rlang::expr(38.5 < Temperature),
                tag = "Temperature")


  # check model
  # check here with before (as nothing should have changed on model)
  expect_equal(model_state_equal("./csv/observed/before1", model, query), TRUE)
})
test_that("observed works on events", {
  setup_new_env()

  # import measures
  load_env_csv("./csv/observed/before2")

  # observed
  model <- observed(analysr_env, Hemorrhage)


  query <- list(condition = rlang::expr(Hemorrhage), tag = "Hemorrhage")


  # check model
  # check here with before (as nothing should have changed on model)
  expect_equal(model_state_equal("./csv/observed/before2", model, query), TRUE)
})
test_that("observed works on desciption without value", {
  setup_new_env()

  # import measures
  load_env_csv("./csv/observed/before3")

  # observed
  model <- observed(analysr_env, Covid_Positive)


  query <- list(condition = rlang::expr(Covid_Positive), tag = "Covid_Positive")


  # check model
  # check here with before (as nothing should have changed on model)
  expect_equal(model_state_equal("./csv/observed/before3", model, query), TRUE)
})
