test_that("induce_event works", {
  setup_new_env()

  load_env_csv("./csv/induce_event/before")

  # induce period
  induce_event(analysr_env, Temperature > 37.5, "Hospitalisation")

  save_env_csv("./csv/induce_event/after")
  expect_equal(model_state_equal("./csv/induce_event/after"), TRUE)
})
test_that("induce_event works with reverse condition", {
  setup_new_env()

  load_env_csv("./csv/induce_event/before")

  # induce period
  induce_event(analysr_env, 37.5 < Temperature, "Hospitalisation")

  expect_equal(model_state_equal("./csv/induce_event/after"), TRUE)
})
