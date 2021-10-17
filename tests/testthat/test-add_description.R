test_that("add_description works", {
  setup_new_env()

  # import model
  load_env_csv("./csv/add_description/before")

  add_description(c(1, 6), "Fever")


  # check model
  expect_equal(model_state_equal("./csv/add_description/after1", analysr_env), TRUE)

})

test_that("add_description works with space", {
  setup_new_env()

  # import model
  load_env_csv("./csv/add_description/before")

  add_description(c(2, 7), "Fever after surgery")


  # check model
  expect_equal(model_state_equal("./csv/add_description/after2", analysr_env), TRUE)

})
