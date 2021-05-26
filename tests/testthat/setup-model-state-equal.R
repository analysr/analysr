model_state_equal <- function(after_path) {

  # Create an environment to store after data
  after_env <- new.env(parent = emptyenv())

  result <- TRUE

  # load current_hash
  after_env$current_hash <- as.integer(readr::read_file(file.path(after_path,
                                                          "current_hash.txt")))

  # check current_hash
  if (after_env$current_hash != analysr_env$current_hash) {
    result <- paste0("Variable current_hash does not match:\n current: ",
                     analysr_env$current_hash, "\n expected: ",
                     after_env$current_hash)
    stop(result)
  }


  # load dataframes
  df_to_load <- c("measures",  "events", "stat_units",
                  "descriptions", "periods")

  quiet_read_csv <- purrr::quietly(readr::read_csv)

  df_to_load %>%
    purrr::map(function(x) {
      file_path <- file.path(after_path, paste0(x, ".csv"))
      result_csv <- quiet_read_csv(file = file_path,
                                   col_types = readr::cols("hash" = "i",
                                   "stat_unit" = "c"))$result
      result_csv <- as.data.frame(result_csv)

      assign(x, result_csv, envir = after_env)
    })

  # check data_frames
  for (df in df_to_load) {
    actual <- getElement(analysr_env, df)
    actual[is.na(actual)] <- "NA"
    expected <- getElement(after_env, df)
    expected[is.na(expected)] <- "NA"
    valid <- dplyr::all_equal(expected,expected)
    if (valid != TRUE) {
        result <- paste0("Table ", df, " does not match:\n", valid)
        print("Actual data frame:")
        print(actual)
        print("Expected data frame:")
        print(expected)
        stop(result)
    }
  }

  result
}
