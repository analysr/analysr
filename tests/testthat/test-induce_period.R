

test_that("induce_period works", {
  setup_new_env()

  # import measures
  load_env_csv("./csv/induce_period/before")

  # induce period
  induce_period(Temperature > 37.5, "Fever", 1*days)

  # check model
  expect_equal(model_state_equal("./csv/induce_period/after"), TRUE)
})
