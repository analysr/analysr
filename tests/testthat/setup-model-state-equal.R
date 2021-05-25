model_state_equal <- function(after_path) {

  # Create an environment to store after data
  after_env <- new.env(parent = emptyenv())

  # load dataframes
  df_to_load <- c("measures", "periods", "events", "stat_units", "descriptions")

  quiet_read_csv <- purrr::quietly(readr::read_csv)

  df_to_load %>%
    purrr::map(function(x) {
      file_path <- file.path(after_path, paste0(x, ".csv"))
      result_csv <- quiet_read_csv(file = file_path)$result
      result_csv <- as.data.frame(result_csv)

      assign(x, result_csv, envir = after_env)
    })

  # load current_hash
  after_env$current_hash <- as.numeric(
                        readr::read_file(file.path(save_path, "current_hash")))

  # TODO: Throw an error if a table does not match
  # Compare after_env and analysr_env
  # Use `stop` func https://stackoverflow.com/a/1608170/6579059
}