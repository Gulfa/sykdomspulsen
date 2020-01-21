#' create_input_directories
#' This function is used to create all the necessary input directories
create_input_directories <- function(){
  fs::dir_create("/input/norsyss")
  fs::dir_create("/input/normomo")
}
