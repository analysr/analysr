#' Add stat units
#'
#'
#' Add to stat_units table
#'
#' @param ids A vector containing ids of stat units to add.
#' @param model An AnalysR env.
#' Default: `analysr_env`
#' @examples
#' add_stat_units(c(144545, 78787, 845))
#' @export
add_stat_units <- function(ids, model = analysr_env) {
    to_add <- unique(ids[!(ids %in% model$stat_units$stat_unit)])
    if (length(to_add) != 0) {
        result <- tibble::tibble(get_hash(length(to_add)), to_add)
        # should define name https://bit.ly/2QvwrdM
        colnames(result) <- c("hash", "stat_unit")
        model$stat_units <- rbind(model$stat_units, result)
    }
    model
}