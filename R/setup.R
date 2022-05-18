#' Setup API Token
#' @description setup global variable of token for notion api
#' 
#' @param file token file
#' @export

token_setup_global <- function(file=system.file("extdata","token.txt",package = "ttnotion")){
  options("token"=readLines(file))
}



#' Setup ID
#' @description setup page or database id of notion as global variable
#' 
#' @param id a hash string of notion page or database id
#' @export

id_setup_global <- function(id){
  options("id"= id)
}