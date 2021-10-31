test_that("inside works on a request", {
  setup_new_env()

  # import model
  load_env_csv("./csv/inside/test")
  result <- (
    analysr_env
    %>% observed(Temperature > 38.5)
    %>% at_most(15 * days)
    %>% inside("Hospitalization")
    %>% before("Surgery")
  )

  #expected_result
  exp_result <- c(7)

  #check result
  expect_equal(result, exp_result)
})
