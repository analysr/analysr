model_state_equal <- function(after_path) {

  # Create an environment to store after data
  after_env <- new.env(parent = emptyenv())

  # load dataframes
  df_to_load <- c("measures", "periods", "events", "stat_units", "descriptions")

  quiet_read_csv <- purrr::quietly(readr::read_csv)

  df_to_load %>%
    purrr::map(function(x) {
      file_path <- file.path(after_path, paste0(x, ".csv"))
      result_csv <- quiet_read_csv(file = file_path,
                                   col_types = readr::cols("hash" = "i"))$result
      result_csv <- as.data.frame(result_csv)

      assign(x, result_csv, envir = after_env)
    })

  # load current_hash
  after_env$current_hash <- as.numeric(readr::read_file(file.path(after_path,
                                                        "current_hash.txt")))
  # TODO: Fix bug with data frame types
  #print(typeof(analysr_env$periods$hash))
  #print(typeof(after_env$periods$hash))

  result <- TRUE

  for (df in df_to_load){
    valid <- dplyr::all_equal(getElement(analysr_env,df),
                              getElement(after_env,df))
    if (valid != TRUE){
      result <- paste0("Table ", df, " does not match:\n", valid)
      stop(result)
    }
  }
  result
}
