

#' Check table integrity
#'
#' A function to check the integrity of analysr tables
check_tables_integrity <- function () {
    # check measures table
    if (!all(colnames(analysr_env$measures) == c("hash", "stat_unit", "date",
                                                "tag", "value", "status"))) {
      warning("Analysr measures table is not consistent")
      return(FALSE)
    }
    # check periods table
    if (!all(colnames(analysr_env$periods) == c("hash", "stat_unit", "begin",
                                              "end", "tag"))) {
      warning("Analysr periods table is not consistent")
      return(FALSE)
    }
    # check events table
    if (!all(colnames(analysr_env$events) == c("hash", "stat_unit",
                                              "date", "tag"))) {
      warning("Analysr events table is not consistent")
      return(FALSE)
    }
    # check descriptions table
    if (!all(colnames(analysr_env$descriptions) == c("hash", "type",
                                                             "value"))) {
      warning("Analysr descriptions table is not consistent")
      return(FALSE)
    }
    # check stat_units table
    if (!all(colnames(analysr_env$stat_units) == c("hash", "stat_unit"))) {
      warning("Analysr stat_units table is not consistent")
      return(FALSE)
    }
    TRUE
}