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
  condition <- rlang::enexpr(condition)
  # print(lobstr::ast(!!condition))

  tag_to_check <- rlang::as_string(condition[[2]])[1]
  operator <- condition[[1]]
  wanted_value <- condition[[3]]
  print(tag_to_check)
  print(subset(analysr_env$measures, tag == tag_to_check))
  print(subset(analysr_env$measures, value > wanted_value))
  #print(eval(rlang::call2(operator, tag_to_check, value)))
}
