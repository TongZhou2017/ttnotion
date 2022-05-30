#' Table to Daily Report
#' @description Generate daily report content based on Notion csv format database
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

#' Notion database object to Daily Report
#' @description Generate daily report content based on Notion database object
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
#' @param object input simplified notion database object
#' @param file input file in csv format
#' @export

object_to_report <- function(object,file){
  tab <- object
  colnames(tab) <- c("Subtopic", "Time", "Details", "Topic", "Date","Location", "Name")
  
  tab <- tab %>% mutate(date = Date %>% 
                          ymd())
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
  cat(lines,file=file)
  
}

#' Mutate new column of start and end time
#' @description based on the database table time column of character format to generate start and end time in POSIXct format
#' 
#' @import dplyr
#' @importFrom stringr str_extract
#' @importFrom stringr str_remove
#' @importFrom lubridate ymd_hms
#' @param tab input data
#' @export

table_mutate_time <- function(tab){
  tab <- tab %>% 
    mutate(start = str_extract(时间,"^.* -- ")) %>% 
    mutate(start = ymd_hms(str_remove(start, " -- "),tz = "Asia/Shanghai")) %>% 
    mutate(end = str_extract(时间," -- .*$")) %>% 
    mutate(end = ymd_hms(str_remove(end, " -- "),tz = "Asia/Shanghai"))
  return(tab)
}

#' Create main table for xiaohongshu graph content
#' @description based on the database table to generate a frame of graph
#' 
#' @importFrom stringr str_wrap
#' @param tab input data
#' @param index table row index
#' @export

table_build_graph <- function(tab,index){
  d = data.frame(x1=c(0,0,3,0,3,0),
                 x2=c(6,3,6,3,6,6),
                 y1=c(4,3,3,2,2,1),
                 y2=c(5,4,4,3,3,2),
                 t=c("title","start","end","location","subtopic","detail"),
                 r=c(paste0(tab$Name[index],collapse = ", "),
                     paste0(tab[index,"start"],collapse = ", "),
                     paste0(tab[index,"end"],collapse = ", "),
                     paste0(tab[index,"地点"],collapse = ", "),
                    str_wrap(paste0(tab[index,"类别"],collapse = ", "),40),
                    paste0(tab[index,"详情"],collapse = ", "))
                )
  return(d)
}