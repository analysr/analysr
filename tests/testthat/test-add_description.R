test_that("add_description works", {
  setup_new_env()

  # import model
  load_env_csv("./csv/add_description/before")

  add_description(c(1, 6), "Fever")


  # check model
  expect_equal(model_state_equal("./csv/add_description/after", analysr_env), TRUE)

})
