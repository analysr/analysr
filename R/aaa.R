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
} 
setup_new_env()