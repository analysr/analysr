test_that("save_env_csv works", {
  # reset env
  setup_new_env()

  save_env_csv("./tmp/save_env_csv/test/")

  # check if dir exist
  # (should be created by the function)
  expect_equal(dir.exists("./tmp/save_env_csv/test/"), TRUE)
  # check if files exists
  expect_equal(file.exists("./tmp/save_env_csv/test/current_hash"), TRUE)
  expect_equal(file.exists("./tmp/save_env_csv/test/events.csv"), TRUE)
  expect_equal(file.exists("./tmp/save_env_csv/test/descriptions.csv"), TRUE)
  expect_equal(file.exists("./tmp/save_env_csv/test/periods.csv"), TRUE)
  expect_equal(file.exists("./tmp/save_env_csv/test/stat_units.csv"), TRUE)

})
