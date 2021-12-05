test_that("extract_feature works", {
  setup_new_env()

  # import measures
  load_env_csv("./csv/extract_feature/before1")

  # restrict
  model <- extract_feature(analysr_env, "Gender")

  # check model
  expect_equal(model_state_equal("./csv/extract_feature/after1", model), TRUE)
})


test_that("extract_feature works when there is some missing values", {
  setup_new_env()

  # import measures
  load_env_csv("./csv/extract_feature/before2")

  # restrict
  model <- extract_feature(analysr_env, "Gender")

  # check model
  expect_equal(model_state_equal("./csv/extract_feature/after2", model), TRUE)
})
