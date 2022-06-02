#' Get file dir
#' @description Get file dir from string
#' @param str str
#' @param up up dir
#' @import stringr
#' @export

file_get_dir <- function(str,up=FALSE){
  if(up){
    str <- stringr::str_extract(str,"^.*[/]")
    str <- stringr::str_remove(str,"/$")
    str <- stringr::str_extract(str,"^.*[/]")
  }else{
    str <- stringr::str_extract(str,"^.*[/]")
  }
  return(str)
}