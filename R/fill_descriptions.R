#' Fill descriptions
#'
#' A function to fill descriptions table when importing data
#' (typically those which are not in mandatory)
#'
#' @param hash a vector containing hash to associate
#' @param types a vector of strings (the name of the columns to import)
#' @param data the data frame you want to extract data from
#' @param n (optional) length of hash if already calculated
fill_descriptions <- function(hashes, types, data, n = length(hash)) {

    prepare_data <- dplyr::select(data,dplyr::all_of(types))
    prepare_data <- dplyr::mutate(prepare_data, hash = hashes,
                    .before = types[1])

    result <- tidyr::pivot_longer(prepare_data,
                                  cols = types,
                                  names_to = "type",
                                  values_to = "value")
    analysr_env$descriptions <- dplyr::bind_rows(analysr_env$descriptions,
                                result)
}