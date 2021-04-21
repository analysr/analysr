#' Add stat units
#'
#' Add to stat_units table
#'
#' @param ids A vector containing ids of stat units to add.
#' @examples
#' add_stat_units(c(144545, 78787, 845))
add_stat_units <- function(ids) {
    to_add <- ids[!(ids %in% analysr_env$stat_units$stat_unit)]
    if (length(to_add) != 0) {
        result <- data.frame(get_hash(length(to_add)), to_add)
        # should define name https://bit.ly/2QvwrdM
        colnames(result) <- c("hash", "stat_unit")
        analysr_env$stat_units <- rbind(analysr_env$stat_units, result)
    }
}