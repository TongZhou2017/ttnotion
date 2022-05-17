#' Table to Daily Report
#' Generate daily report content based on Notion csv format database
#' 
#' @import dplyr
#' @importFrom lubridate today
#' @importFrom lubridate month
#' @importFrom lubridate mday
#' @importFrom lubridate mdy
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_remove
#' @importFrom stringr str_replace
#' @importFrom ttfriends table_split_group
#' @importFrom ttdiv file_get_dir
#' @importFrom utils read.table
#' @param file input file in csv format
#' @export

table_to_report <- function(file){
  
  # reading dir of input for output
  file_dir <- file_get_dir(file)
  
  # reading database in csv format and convert date column into lubridate type which easy for sort and filter
  tab <- read.table(file,sep=",",header=T,stringsAsFactors=F)
  colnames(tab) <- c("Name",	"Topic", "Subtopic", "Time", "Location", "Details", "Date")
  tab <- tab %>% mutate(date = str_replace_all(Date," ","-") %>% 
                          str_remove(",") %>% 
                          mdy())
  tab_today <- tab %>% filter(date == today())
  
  # get topic and subtopic information as tag format
  topic <- table_split_group(tab_today,"Topic",sep=", ") %>% 
    pull(Topic) %>% 
    unique() %>% 
    str_replace("^","#") %>% 
    paste0(collapse="")
  subtopic <- table_split_group(tab_today,"Subtopic",sep=", ") %>% 
    pull(Subtopic) %>% 
    unique() %>% 
    str_replace("^","#") %>% 
    paste0(collapse="")
  
  # combine and wirte report information for social media
  lines <- paste0("[",month(today()),".",mday(today()),"]  ",nrow(tab_today)," Reports\n\nTopics\n\n",topic,"\n\nSubtopics\n\n",subtopic,"\n")
  cat(lines,file=paste0(file_dir,"daily_report_",today(),".txt"))
  
}

