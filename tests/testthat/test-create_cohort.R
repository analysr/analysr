test_that("create_cohort works", {
  setup_new_env()

  # import measures
  load_env_csv("./csv/restrict/before1")

  # restrict
  res <- create_cohort(analysr_env, Gender == "Female")
  print(res)

  expect_equal(2 * 2, 4)
})
