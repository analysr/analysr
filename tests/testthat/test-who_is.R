test_that("who_is works on a request", {
  setup_new_env()

  # import model
  load_env_csv("./csv/who_is/test")
  result <- (
    analysr_env
    %>% observed(Temperature > 38.5)
    %>% at_most(15 * days)
    %>% who_is(Gender == "Male")
    %>% before("Surgery")
  )

  #expected_result
  exp_result <- c(1)

  #check result
  expect_equal(result, exp_result)
})
test_that("who_is works on a request with a vector", {
  setup_new_env()

  # import model
  load_env_csv("./csv/who_is/test")
  result <- (
    analysr_env
    %>% observed(Temperature > 38.5)
    %>% at_most(15 * days)
    %>% before("Surgery")
    %>% who_is(Gender == "Male")
  )

  #expected_result
  exp_result <- c(1)

  #check result
  expect_equal(result, exp_result)
})
