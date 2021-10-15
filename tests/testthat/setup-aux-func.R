quiet_read_csv <- purrr::quietly(readr::read_csv)
show_env <- function(env) {
  print(sapply(ls(env), function(x) get(x, envir = env)))
}
