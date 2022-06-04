#' Graph for xiaohongshu main content
#' @description single unit of main content
#' 
#' @import ggplot2
#' @importFrom ggthemes theme_solid
#' @importFrom grDevices png	
#' @importFrom grDevices dev.off
#' @param data input data frame
#' @param file output file
#' @export

graph_main_single <- function(data,file=NULL){
  p <- ggplot() + 
    scale_x_continuous(name="x") + 
    scale_y_continuous(name="y") +
    geom_rect(data=data, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=t), color="black", alpha=0.5) +
    geom_text(data=data, aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=r), size=3) +
    ggthemes::theme_solid() +
    theme(legend.position = "none")
  if(is.null(file)){
    return(p)
  }else{
    png(file,res = 900,width = 6400,height = 3600)
    print(p)
    dev.off()
  }
}

#' Graph for xiaohongshu cover content
#' @description update cover content
#' 
#' @importFrom lubridate today
#' @importFrom lubridate month
#' @importFrom lubridate day
#' @importFrom meme meme
#' @importFrom grDevices png	
#' @param data input data frame
#' @param input raw cover graph
#' @param output updated cover graph output dir
#' @export

graph_cover_update <- function(data,input=system.file("extdata","cover.png",package = "ttnotion"),output){
  str_date <- paste0(month(today()),".",day(today()))
  p <- meme(input,paste0(str_date,"共有",nrow(data),"个讲座"),vjust = 0.5,color = "orange",size = 1.5)
  png(paste0(output,"/cover_",str_date,".png"),res = 600,height = 2400,width = 1800)
  print(p)
  dev.off()
}

#' Graph for xiaohongshu main content by topic
#' @description combine main content by topic
#' 
#' @importFrom patchwork wrap_plots
#' @importFrom patchwork plot_annotation
#' @importFrom stringr str_remove
#' @importFrom grDevices png	
#' @importFrom grDevices dev.off
#' @param data input data frame
#' @param output output dir
#' @param pages output pages number
#' @export

graph_main_topic <- function(data,output,pages=NULL){
  topics <- unique(data$大类)
  str_date <- paste0(month(today()),".",day(today()))
  if(is.null(pages)){
    for (index_topic in 1:length(topics)) {
      data_group_by_topic <- data %>% filter(大类 == topics[index_topic])
      p <- list()
      str_list_wrap <- NULL
      for (i in 1:nrow(data_group_by_topic)) {
        d <- table_build_graph(data_group_by_topic,i)
        p[[i]] <- graph_main_single(d)
        str_list_wrap <- paste0(str_list_wrap, paste0("p[[",i,"]]",sep=","))
      }
      str_list_wrap_command <- paste0("p <- wrap_plots(",str_list_wrap,'ncol = 1) + plot_annotation(title = paste0(str_date," 【",','topics[index_topic],','"】讲座"))')
      png(paste0(output,"/daily_report_",str_date,"_",topics[index_topic],".png"),res = 300,width = 1800,height = nrow(data_group_by_topic) * 600)
      eval(parse( text=str_list_wrap_command ))
      print(p)
      dev.off()
    }
  }else{
    rank <- table(data$大类) %>% as.data.frame() %>% arrange(desc(Freq))
    colnames(rank)[1] <- "topic"
    top <- rank[1:(pages-1),]
    other <- rank[pages:nrow(rank),]
    for (index_topic in 1:nrow(top)) {
      data_group_by_topic <- data %>% filter(大类 == top$topic[index_topic])
      p <- list()
      str_list_wrap <- NULL
      for (i in 1:nrow(data_group_by_topic)) {
        d <- table_build_graph(data_group_by_topic,i)
        p[[i]] <- graph_main_single(d)
        str_list_wrap <- paste0(str_list_wrap, paste0("p[[",i,"]]",sep=","))
      }
      str_list_wrap_command <- paste0("p <- wrap_plots(",str_list_wrap,'ncol = 1) + plot_annotation(title = paste0(str_date," 【",','top$topic[index_topic],','"】讲座"))')
      png(paste0(output,"/daily_report_",str_date,"_",top$topic[index_topic],".png"),res = 300,width = 1800,height = nrow(data_group_by_topic) * 600)
      eval(parse( text=str_list_wrap_command ))
      print(p)
      dev.off()
    }
    p_other <- list()
    str_list_wrap_other <- list()
    str_list_wrap_heights <- NULL
    for (index_topic_other in 1:nrow(other)) {
      data_group_by_topic <- data %>% filter(大类 == other$topic[index_topic_other])
      p <- list()
      str_list_wrap <- NULL
      for (i in 1:nrow(data_group_by_topic)) {
        d <- table_build_graph(data_group_by_topic,i)
        p[[i]] <- graph_main_single(d)
        str_list_wrap <- paste0(str_list_wrap, paste0("p[[",i,"]]",sep=","))
      }
      str_list_wrap_command <- paste0("p_other[[index_topic_other]] <- wrap_plots(",str_list_wrap,"ncol = 1) + plot_annotation(title = paste0(str_date,' 【',",'other$topic[index_topic],',"'】讲座'))")
      #png(paste0(output,"/daily_report_",str_date,"_",top$topic[index_topic],".png"),res = 300,width = 1800,height = nrow(data_group_by_topic) * 600)
      eval(parse( text=str_list_wrap_command ))
      str_list_wrap_other <- paste0(str_list_wrap_other, paste0("p_other[[",index_topic_other,"]]",sep=","))
      #dev.off()
      str_list_wrap_heights <- paste0(str_list_wrap_heights,",",nrow(data_group_by_topic))
    }
    str_list_wrap_heights <- str_remove(str_list_wrap_heights,"^,")
    str_list_wrap_command_other <- paste0("p_other <- wrap_plots(",str_list_wrap_other,'ncol = 1, heights = c(',str_list_wrap_heights,'))')
    cat(str_list_wrap_command_other)
    eval(parse( text=str_list_wrap_command_other ))
    png(paste0(output,"/daily_report_",str_date,"_other.png"),res = 300,width = 1800,height = sum(other$Freq) * 600)
    print(p_other)
    dev.off()
  }
}


#' Timeline graph
#' @description timeline graph separeted by topic and sort by start time
#' 
#' @import RColorBrewer
#' @import vistime
#' @importFrom lubridate today
#' @importFrom lubridate month
#' @importFrom lubridate day
#' @importFrom grDevices png	
#' @importFrom grDevices dev.off
#' @importFrom grDevices colorRampPalette
#' @import dplyr
#' @param tab input table
#' @param output output dir
#' @export

graph_timeline <- function(tab,output=NULL){
  str_date <- paste0(month(today()),".",day(today()))
  tab <- tab %>% arrange(start)
  value_new <- colorRampPalette(brewer.pal(name="Set1", n = 8))(length(unique(tab$大类)))
  value_old <- unique(tab$大类)
  tab$color <- replace.values(search = value_old, replace = value_new, x=tab$大类)
  lims <- c(min(tab$start) - 2*60*60,max(tab$start) + 2*60*60)
  p <- gg_vistime(tab,events="Name",groups="大类",start="start",end="end",optimize_y = F) + scale_x_datetime(date_breaks = '1 hours',date_labels = '%H',limits = lims) + theme(panel.grid.minor.x = element_line(linetype = "dashed"))+labs(title = paste0(str_date,"讲座时间线"))
  if(is.null(output)){
    return(p)
  }else{
    png(paste0(output,"/timeline_",str_date,".png"),res = 300,height = 1800,width = 5200)
    print(p)
    dev.off()
  }
}


#' Data frame replace elements
#' @description using same length vector to replace data frame elements. from https://stackoverflow.com/questions/11810605/replace-contents-of-factor-column-in-r-dataframe
#' 
#' @param search old value
#' @param replace new value
#' @param x input data
#' @export

replace.values <- function(search, replace, x){
  stopifnot(length(search) == length(replace))
  xnew <- replace[ match(x, search) ]
  takeOld <- is.na(xnew) & !is.na(x)
  xnew[takeOld] <- x[takeOld]
  return(xnew)
}


