#' induce_period
#'
#' Create period from measures table depending on a condition,
#' and add them to periods table
#'
#' @return The part of  that have been created.
#'
#' @param condition A condition (to be defined).
#' @param tag_to_create A string for the tag to add.
#' @param duration A duration.
#'
#' @export
induce_period <- function(condition, tag_to_create, duration) {
  #Test de modification de paul
  condition <- rlang::enexpr(condition)
  # Here we admit that a condition is like: tag operator value
  # e.g. Temperature > 37.5

  # Lorsque la condition est en deux parties avec un element de logique
  if (length(condition) > 2){
    print("Methode avec opérateur")

      tag_to_check <- rlang::as_string(condition[[2]])[1]
      print(tag_to_check)
      operator <- condition[[1]]
      wanted_value <- condition[[3]]

      data <- subset(analysr_env$measures, tag == tag_to_check)
      data <- data[eval(rlang::call2(operator, data$value, wanted_value)),]
  print(data)
      n <- nrow(data)

      result <- data.frame(hash = get_hash(n), stat_unit=data$stat_unit,
                           begin=data$date, end=data$date+duration, desc=rep(tag_to_create,n))

      analysr_env$periods <- rbind(analysr_env$periods, result)
  }

  # Lorsque la condition est unique
  else if (length(condition > 0)){
    print("Methode sans opérateur")

    tag_to_check <- rlang::as_string(toString(condition))
    print(tag_to_check)

    data <- subset(analysr_env$measures, tag == tag_to_check)
    print(data)

    n <- nrow(data)

    result <- data.frame(hash = get_hash(n), stat_unit=data$stat_unit,
                         begin=data$date, end=data$date+duration, desc=rep(tag_to_create,n))

    analysr_env$periods <- rbind(analysr_env$periods, result)
  }

  #Sinon
  else {
    print("Methode non codée")
  }

}
