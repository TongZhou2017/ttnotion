#' Pipeline xiaohongshu
#' automatic workflow to generate content for social media based on Notion database
#' 
#' @importFrom showtext showtext_auto
#' @param dir output dir
#' @param id database id
#' @param token token file
#' @export

pipeline_notion_xiaohongshu <- function(dir,id="3b924c63c237440c854cdace3f254bd3",token=system.file("extdata","token.txt",package = "ttnotion")){
  showtext::showtext_auto()
  token_setup_global(token)
  id_setup_global(id)
  database_list <- ttnotion::database_query()
  database_table <- notion_database_table(database_list)
  object_to_report(database_table,dir = dir)
  database_table_today <- database_table %>% filter(日期 == today())
  database_table_today <- table_mutate_time(database_table_today)
  graph_cover_update(database_table_today,output = dir)
  graph_timeline(database_table_today,dir)
  graph_main_topic(database_table_today,dir,3)
}