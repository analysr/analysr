analysr_env <- new.env(parent = emptyenv())

#' Setup new envirenment
#'
#' @examples
#' setup_new_env()
#' @export
setup_new_env <- function() {
    # create data frame for measures
    analysr_env$measures <- data.frame(matrix(ncol = 6, nrow = 0))

    colnames(analysr_env$measures) <- c("hash", "stat_unit", "date",
                                        "tag", "value", "status")

    # create data frame for periods
    analysr_env$periods <- data.frame(matrix(ncol = 5, nrow = 0))
    colnames(analysr_env$periods) <- c("hash", "stat_unit", "begin",
                                        "end", "desc")

    # create data frame for events
    analysr_env$events <- data.frame(matrix(ncol = 4, nrow = 0))
    colnames(analysr_env$events) <- c("hash", "stat_unit", "date", "tag")

    # create data frame for stat_units
    analysr_env$stat_units <- data.frame(matrix(ncol = 2, nrow = 0))
    colnames(analysr_env$stat_units) <- c("hash", "stat_unit")

    # create data frame for descriptions
    analysr_env$descriptions <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(analysr_env$descriptions) <- c("hash", "type", "value")

    # define current hash not used
    analysr_env$current_hash <- as.integer(1)

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
#' @export
get_hash <- function(n) {
    if (n == 1) {
        result <- analysr_env$current_hash
        analysr_env$current_hash <- 1 + analysr_env$current_hash
    } else {
        result <- analysr_env$current_hash:(n - 1 + analysr_env$current_hash)
        analysr_env$current_hash <- n + analysr_env$current_hash
    }
    as.integer(result)
}
