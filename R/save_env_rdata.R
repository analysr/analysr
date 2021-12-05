#' Save env RData
#'
#' Save current environment in a Rdata file
#'
#' @param save_path A path to save current environment
#' (existence is required)
#' @param model a AnalysR Model (default : `analysr_env`)
#' @examples
#' save_env_rdata("~/")
#'
#' @import dplyr
#'
save_env_rdata <- function(save_path, model = analysr_env) {
  if (missing(save_path)) {
    # defines save path
    save_path <- getwd()
  }
  # defines name (using a timestamp)
  save_name <- paste0(floor(as.numeric(Sys.time())), "-save.RData")
  save_file <- file.path(save_path, save_name)

  save(model, file = save_file)
}
