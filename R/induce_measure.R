
convert <- function(ee) {
  tried <- try(ee <- rlang::as_string(ee), silent = TRUE)
  if(!inherits(tried, "try-error")) {
    ee
  }
}

get_AST <- function(ee) purrr::map_if(as.vector(ee), is.call, get_AST, .else = convert)

get_tags <- function(calcul) {
  len <- length(calcul)
  if (len > 1) {
      vect <- c()
      for (i in 2:len) {
        if (typeof(calcul[i][[1]]) == "character") {
          vect <- c(vect, calcul[i][[1]])
        } else {
          vect <- c(vect, get_tags(calcul[i][[1]]))
        }
      }
      return(vect)
  }
  c()
}


replace_values <- function(string, tags_arr, values_arr) {
  for (i in 1:length(tags_arr)) {
    string <- stringr::str_replace_all(string, tags_arr[i], toString(values_arr[i]))
  }
  string
}

#' induce_measure
#'
#' @param model An AnalysR env
#' @param tag_to_create Label to write in measure table
#' @param calcul A function containing at least a measures tag
#' @param tag_ref A tag that is considered to iter on measures table
#'
#' @export
induce_measure <- function(model, tag_to_create, calcul, tag_ref) {
  calcul <- rlang::enexpr(calcul)
  calcul_as_string <- toString(calcul)
  calcul <- get_AST(calcul)
  tags <- get_tags(calcul)
}
