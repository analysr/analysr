test_that("from works", {
  setup_new_env()

  # import model
  load_env_csv("./csv/from_to/test")
  model <- observed(analysr_env, Temperature > 38.5)

  # from
  model <- from(model, as.Date("2006-10-22"))

  # TODO: how to get date ? which parse system ?

  query <- list(condition = rlang::expr(Temperature > 38.5),
                tag = "Temperature",
                from_date = as.Date("2006-10-22"))

  # check model (model should not have changed, only query has)

  expect_equal(model_state_equal("./csv/from_to/test", model, query), TRUE)
})
