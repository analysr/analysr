analysr_env <- new.env(parent = emptyenv())

setup_new_env <- function() {
    # create data frame for measures
    analysr_env$measures <- data.frame(matrix(ncol = 4, nrow = 0))
    colnames(analysr_env$measures) <- c("stat_unit", "date", "tag", "value")

    # create data frame for periods
    analysr_env$periods <- data.frame(matrix(ncol = 4, nrow = 0))
    colnames(analysr_env$periods) <- c("stat_unit", "begin", "end", "desc")

    # create data frame for events
    analysr_env$events <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(analysr_env$events) <- c("stat_unit", "date", "tag")

    # define current hash not used
    analysr_env$current_hash <- 0

}
setup_new_env()

#' Get a hash
#'
#' Return a number or verctor depending on `n` value
#'
#' @return A number or verctor depending on `n` value
#'
#' @param n A number.
#' @examples
#' get_hash(10)
get_hash <- function(n) {
    if (n == 1) {
        result <- analysr_env$current_hash
        analysr_env$current_hash <- 1 + analysr_env$current_hash
    } else {
        result <- analysr_env$current_hash:(n - 1 + analysr_env$current_hash)
        analysr_env$current_hash <- n + analysr_env$current_hash
    }
    result
}