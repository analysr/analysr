test_that("save_env_rdata works", {
  test_folder <- file.path(getwd(), "save_env_rdata_tmp/")
  # create test folder (if not exist)
  if(!dir.exists(test_folder)) {
    dir.create(test_folder, showWarnings = FALSE)
  }

  save_env_rdata("./save_env_rdata_tmp/")

  # check if file exist
  expect_equal(length(Sys.glob(file.path(test_folder, "*.RData"))), 1)
  # remove test folder
  unlink(test_folder, recursive = TRUE, force = TRUE)
})
