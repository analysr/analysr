test_that("save_env_csv works", {
  test_folder <- file.path(getwd(), "save_env_csv_tmp/")
  # create test folder (if not exist)
  if(!dir.exists(test_folder)) {
    dir.create(test_folder, showWarnings = FALSE)
  }

  save_env_csv("./save_env_csv_tmp/test/")

  # check if dir exist
  # (schould be created by the function)
  expect_equal(dir.exists("./save_env_csv_tmp/test/"), TRUE)
  # check if files exists
  expect_equal(file.exists("./save_env_csv_tmp/test/current_hash"), TRUE)
  expect_equal(file.exists("./save_env_csv_tmp/test/events.csv"), TRUE)
  expect_equal(file.exists("./save_env_csv_tmp/test/descriptions.csv"), TRUE)
  expect_equal(file.exists("./save_env_csv_tmp/test/periods.csv"), TRUE)
  expect_equal(file.exists("./save_env_csv_tmp/test/stat_units.csv"), TRUE)

  # remove test folder
  unlink(test_folder, recursive = TRUE, force = TRUE)
})
