#' Save env csv
#'
#' Save current environment in a folder containing CSV
#'
#' @param save_path A path to save current environment
#' (existence is not required)
#' @examples
#' save_env_csv("~/")
#' 
#' @import dplyr
#' @importFrom utils write.csv
#' 
#' @export 
save_env_csv <- function(save_path) {

  if (missing(save_path)) {
    # defines name (using a timestamp)
    save_name <- paste0(floor(as.numeric(Sys.time())), "-save")

    # defines save path
    save_path <- file.path(getwd(), save_name)
  }


  # create save folder (if not exist)
  if (!dir.exists(save_path)) {
    dir.create(save_path, showWarnings = FALSE)
  }

  # save dataframes
  df_to_save <- c("measures", "periods", "events", "stat_units", "descriptions")


  df_to_save %>%
    purrr::map(function(x) {
      file_path <- file.path(save_path, paste0(x, ".csv"))
      write.csv(getElement(analysr_env, x), file_path, row.names = FALSE)
    })

  # save current_hash
  write(analysr_env$current_hash, file.path(save_path, "current_hash"))

  # maybe it will be good to add a better way to save variable
}
