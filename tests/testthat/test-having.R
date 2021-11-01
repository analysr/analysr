test_that("having works on a request", {
  setup_new_env()

  # import model
  load_env_csv("./csv/having/test")
  result <- (
    analysr_env
    %>% observed(Temperature > 38.5)
    %>% at_most(15 * days)
    %>% having(Gender == "Male")
    %>% before("Surgery")
  )

  #expected_result
  exp_result <- c(1)

  #check result
  expect_equal(result, exp_result)
})
