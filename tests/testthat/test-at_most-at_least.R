test_that("at_most works for 15 days", {
  setup_new_env()

  # import model
  load_env_csv("./csv/at_most/model")
  model <- observed(analysr_env, Temperature > 38.5)

  # at_most
  model <- at_most(model, 15*days)



  query <- list(condition = rlang::expr(Temperature > 38.5),
                tag = "Temperature",
                duration_type = "at_most",
                duration = lubridate::duration(15, "days"))

  # check model (model should not have changed, only query has)
  expect_equal(model_state_equal("./csv/at_most/model", model, query), TRUE)
})
test_that("at_most works for compelex date", {
  setup_new_env()

  # import model
  load_env_csv("./csv/at_most/model")
  model <- observed(analysr_env, Temperature > 38.5)

  # at_most
  model <- at_most(model, 15*days+12*(1*week+1*second))



  query <- list(condition = rlang::expr(Temperature > 38.5),
                tag = "Temperature",
                duration_type = "at_most",
                duration = lubridate::duration(15+7*12, "days") + lubridate::duration(12, "seconds"))

  # check model (model should not have changed, only query has)
  expect_equal(model_state_equal("./csv/at_most/model", model, query), TRUE)
})
test_that("at_least works", {
  setup_new_env()

  # import model
  load_env_csv("./csv/at_most/model")
  model <- observed(analysr_env, Temperature > 38.5)

  # at_least
  model <- at_least(model, 15*days)



  query <- list(condition = rlang::expr(Temperature > 38.5),
                tag = "Temperature",
                duration_type = "at_least",
                duration = lubridate::duration(15, "days"))

  # check model (model should not have changed, only query has)

  expect_equal(model_state_equal("./csv/at_most/model", model, query), TRUE)
})
