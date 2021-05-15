#' Load env RData
#'
#' Load saved environment from a Rdata file
#'
#' @param save_path A path to the save file
#'
#' @export
load_env_rdata <- function(save_path) {
    load(save_path)
}
