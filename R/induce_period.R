#' induce_period
#'
#' Create period from measures table depending on a condition, and add them to periods table
#'
#' @return The part of  that have been created.
#'
#' @param condition A condition (to be defined).
#' @param tag_to_create A string for the tag to add.
#' @param duration A duration.
#'
#' @export
induce_period <- function(condition, tag_to_create, duration) {
  condition <- expr(condition)
  tag_to_check <- as_string(condition[[2]])
  operator <- condition[[1]]
  value <- condition[[3]]
  # ast(!!condition)
}
