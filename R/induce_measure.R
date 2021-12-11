
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
  string <- stringr::str_dup(string, 1)
  for (i in 1:length(tags_arr)) {
    string <- stringr::str_replace_all(string, tags_arr[i],
                                               toString(values_arr[i]))
  }
  string
}

#' induce_measure
#'
#' @return A AnalysR model
#'
#' @param model An AnalysR env.
#' Default: `analysr_env`
#' @param tag_to_create Label to write in measures table
#' @param calcul A function containing at least a measures tag
#' @param tag_ref A tag that is considered to iter on measures table
#'
#' @export
induce_measure <- function(model = analysr_env, tag_to_create, calcul,
                          tag_ref) {

  if (model$space_to_underscore) {
    tag_to_create <- gsub(" ", "_", tag_to_create)
  }

  calcul <- rlang::enexpr(calcul)
  calcul_as_string <- deparse(calcul)
  calcul <- get_AST(calcul)
  tags <- get_tags(calcul)

  if (length(tags) == 0) {
    stop("No tag found")
  }

  if (missing(tag_ref)) {
    tag_ref <- tags[1]
  }

  tmp <- dplyr::filter(model$measures, tag == tag_ref)
  for(i in 1:nrow(tmp)) {
    entry <- tmp[i, ]
    # Search other data
    other_data <- dplyr::filter(model$measures, tag %in% tags,
                                          tag != tag_ref,
                                          stat_unit == entry$stat_unit,
                                          date < entry$date)
    # Order data by time desc order
    other_data <- other_data[rev(order(other_data$date)), ]
    other_data <- dplyr::slice(dplyr::group_by(other_data, tag),
                                which.max(date))

    # TODO: add more flexibility (maybe other data can be better after the
    # tag_ref)

    # Evaluate
    tags_arr <- c(entry$tag, other_data$tag)
    values_arr <- c(entry$value, other_data$value)

    to_eval <- replace_values(calcul_as_string, tags_arr, values_arr)

    value <- eval(str2expression(to_eval))
    # TODO: add way to round this value

    new_entry <- tibble::tibble(hash = get_hash(),
                                    stat_unit = entry$stat_unit,
                                    date = entry$date,
                                    tag  = tag_to_create,
                                    value = value,
                                    status = NA)

    model$measures <- rbind(model$measures, new_entry)
  }

  model
}
