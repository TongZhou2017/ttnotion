#' Query Notion database
#' 
#' @import httr
#' @importFrom rjson fromJSON
#' @param id Notion database id
#' @param full full database or head 100 content
#' @export

database_query <- function(id = getOption("id"),full = TRUE){
  url <- paste0("https://api.notion.com/v1/databases/",id,"/query")
  payload <- "{\"page_size\":100}"
  encode <- "json"
  response <- VERB("POST", url, body = payload, add_headers("Notion-Version" = "2022-02-22", "Authorization" = paste0('Bearer ',getOption("token"))), content_type("application/json"), accept("application/json"), encode = encode)
  result <- fromJSON(content(response, "text"))
  result$all_cursor <- result$next_cursor
  while(result$has_more){
    payload <- paste0("{\"page_size\":100,\"start_cursor\":\"",result$next_cursor,"\"}")
    print(result$next_cursor)
    response_loop <- VERB("POST", url, body = payload, add_headers("Notion-Version" = "2022-02-22", "Authorization" = paste0('Bearer ',getOption("token"))), content_type("application/json"), accept("application/json"), encode = encode)
    result_loop <- fromJSON(content(response_loop, "text"))
    result$results <- c(result$results, result_loop$results)
    if(result_loop$has_more){
      result$all_cursor <- c(result$all_cursor,result_loop$next_cursor)
      result$next_cursor <- result_loop$next_cursor
    }else{
      result$has_more <- FALSE
    }
  }
  return(result)
}