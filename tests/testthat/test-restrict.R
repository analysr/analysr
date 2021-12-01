test_that("restrict works", {
  setup_new_env()

  # import measures
  load_env_csv("./csv/restrict/before1")

  # restrict
  model <- restrict(analysr_env, Gender == "Female")

  # check model
  expect_equal(model_state_equal("./csv/restrict/after1", model), TRUE)

})
