#' Load env csv
#'
#'
#' Load saved environment from a folder containing
#'
#' @param save_path A path to the save folder
#' @import dplyr
#'
#' @export
load_env_csv <- function(save_path) {

  # load dataframes
  df_to_load <- c("measures", "periods", "events", "stat_units", "descriptions")

  quiet_read_csv <- purrr::quietly(readr::read_csv)

  df_to_load %>%
    purrr::map(function(x) {
      file_path <- file.path(save_path, paste0(x, ".csv"))
      result_csv <- quiet_read_csv(file = file_path,
                                   col_types = readr::cols("hash" = "i",
                                    "stat_unit" = "c"))$result
      result_csv <- as.data.frame(result_csv)

      assign(x, result_csv, envir = analysr_env)
    })

  # load current_hash
  analysr_env$current_hash <- as.numeric(
                    readr::read_file(file.path(save_path, "current_hash.txt")))

  # maybe it will be good to add a better way to load variable
}
