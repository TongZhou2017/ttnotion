#' Query Notion database
#' 
#' @import httr
#' @importFrom rjson fromJSON
#' @param id Notion database id
#' @export

database_query <- function(id = getOption("id")){
  url <- paste0("https://api.notion.com/v1/databases/",id,"/query")
  payload <- "{\"page_size\":100}"
  encode <- "json"
  response <- VERB("POST", url, body = payload, add_headers("Notion-Version" = "2022-02-22", "Authorization" = paste0('Bearer ',getOption("token"))), content_type("application/json"), accept("application/json"), encode = encode)
  result <- fromJSON(content(response, "text"))
  return(result)
}