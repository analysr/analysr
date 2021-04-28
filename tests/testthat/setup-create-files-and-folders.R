create_folder <- function(subdir) {

  subdir <- file.path(getwd(), paste0("tmp/", subdir))

  fs::dir_create(subdir)

  if(is_testing()) withr::defer(fs::dir_delete(subdir), teardown_env())
}

create_folder("save_env_rdata/")
create_folder("save_env_csv/")

if(is_testing()) withr::defer(fs::dir_delete(file.path(getwd(), "tmp/")), teardown_env(),
                              priority = "last")