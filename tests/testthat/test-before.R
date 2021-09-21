test_that("before works", {
  setup_new_env()

  # import model
  load_env_csv("./csv/before/before")
  model <- observed(analysr_env, Temperature > 38.5)

  # at_most
  model <- at_most(model, 15 * days)

  # before
  model <- before(model, "Surgery")

  query <- list(tag = "Temperature")

  # check model (model should have changed, query also)
  expect_equal(model_state_equal("./csv/before/after", model, query), TRUE)
})
