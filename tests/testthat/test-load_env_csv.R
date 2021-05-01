test_that("load_env_csv works", {
  # reset env
  setup_new_env()

  load_env_csv("./csv/load_env_csv/save/")

  expect_equal(analysr_env$current_hash, 9)

  quiet_read_csv <- purrr::quietly(readr::read_csv)

  df_to_load <- c("measures", "periods", "events", "stat_units", "descriptions")
  df_to_load %>%
    purrr::map(function(x) {
      # check that data frame are not empty
      df <- getElement(analysr_env, x)
      expect_equal(is.data.frame(df) && nrow(df)!=0, TRUE)
      # check that data frame are identical
      file_path <- file.path("./csv/load_env_csv/save/", paste0(x, ".csv"))
      expected <- as.data.frame(quiet_read_csv(file = file_path)$result)
      expect_equal(dplyr::all_equal(df,expected), TRUE)
    })

})
