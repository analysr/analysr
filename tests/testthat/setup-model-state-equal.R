compare_list <- function(a, e) {
  names_a = names(a)
  names_e = names(e)
  if (length(names_a) != length(names_e)) {
    print("Lists does'nt have the same number of value.")
    return(FALSE)
  }
  for (i in names_a) {
      if (is.data.frame(a[i])) {
        if (!dplyr::all_equal(a[i],e[i])) {
          print(paste("The value of", i, "dataframe in query is not equal as expected.", sep = " "))
          print("Actual dataframe:")
          print(a[i])
          print("Expected dataframe:")
          print(e[i])
          return(FALSE)
        }
      } else {
        if (!identical(a[i],e[i])) {
          print(paste("The value of", i, "variable in query is not equal as expected.", sep = " "))
          print("Actual value:")
          print(a[i])
          print("Expected value:")
          print(e[i])
          return(FALSE)
        }
      }
  }
  TRUE
}


model_state_equal <- function(after_path, model, query_expected) {

  if (missing(model)) {
    model <- analysr_env
  }

  # check if query are identical
  if (!missing(query_expected)) {
    if (!compare_list(model$query, query_expected)) {
      stop("Query list does not match")
    }
  }

  # create an environment to store after data
  after_env <- new.env(parent = emptyenv())

  result <- TRUE

  # load current_hash
  after_env$current_hash <- as.integer(readr::read_file(file.path(after_path,
                                                          "current_hash.txt")))

  # check current_hash
  if (after_env$current_hash != model$current_hash) {
    result <- paste0("Variable current_hash does not match:\n current: ",
                     model$current_hash, "\n expected: ",
                     after_env$current_hash)
    stop(result)
  }

  selection_exists <- file.exists(file.path(after_path, "selection.csv"))


  # load dataframes
  df_to_load <- c("measures",  "events", "stat_units",
                  "descriptions", "periods", "selection")

  quiet_read_csv <- purrr::quietly(readr::read_csv)

  df_to_load %>%
    purrr::map(function(x) {
      if (x != "selection" || selection_exists) {
        file_path <- file.path(after_path, paste0(x, ".csv"))
        result_csv <- quiet_read_csv(file = file_path,
                                     col_types = readr::cols("hash" = "i",
                                                             "stat_unit" = "c"))$result
        result_csv <- as.data.frame(result_csv)

        assign(x, result_csv, envir = after_env)
      }
    })

  # check data_frames
  for (df in df_to_load) {
    if (df != "selection" || selection_exists) {
      actual <- getElement(model, df)
      expected <- getElement(after_env, df)
      # convert to same type (TODO: find a better solution)
      actual <- actual %>% mutate_all(as.character)
      expected <- expected %>% mutate_all(as.character)

      valid <- dplyr::all_equal(expected,actual)
      if (valid != TRUE) {
          result <- paste0("Table ", df, " does not match:\n", valid)
          print("Actual data frame:")
          print(actual)
          print("Expected data frame:")
          print(expected)
          stop(result)
      }
    }
  }

  result
}
