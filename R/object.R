#' The notion_data Class
#' @description The notion_data object is an intermediate storage container used internally throughout the integration procedure to hold bits of data that are useful downstream.
#' 
#' @slot results main result database
#' @slot object list or others
#' @slot next_cursor hash id
#' @slot has_more more than query limited, True or False
#' @slot type page or others
#' @slot page list() or others
#' 
#' 
#' @name notion_data-class
#' @rdname notion_data-class
#' @concept object
#' @exportClass notion_data


notion_data <- setClass(
  Class = "notion_data",
  slots = list(
    results = "list",
    object = "character",
    next_cursor = "character",
    has_more = "logical",
    type = "character",
    page = "list"
  )
)

#' Create notion_object automatically
#' @description create notion_object based on type
#' 
#' @param list input list
#' 
#' @export

object_create_auto <- function(list){
  if(list$type == "title"){
    object <- object_create_title(list)
  }
  if(list$type == "multi_select"){
    object <- object_create_multi_select(list)
  }
  if(list$type == "date"){
    object <- object_create_date(list)
  }
  if(list$type == "rich_text"){
    object <- object_create_rich_text(list)
  }
  return(object)
}

#' Simplify notion_data generic method
#' @description for filtering records in date region
#' 
#' @param object notion_data object
#' @rdname notion_database_table
#' @export

setGeneric("notion_database_table", function(object) {
  standardGeneric("notion_database_table")
})

#' Simplify notion_data
#' @description for simplifing database
#' 
#' @importFrom lubridate ymd_hms
#' @importFrom lubridate ymd
#' @importFrom stringr str_detect
#' @param object notion_data object
#' @export

setMethod("notion_database_table",
          "list",# todo notion_data
          function(object){
            # create a empty data frame
            df <- data.frame(matrix(NA,nrow = length(object$results),ncol = length(object$results[[1]]$properties)),stringsAsFactors = F)
            colnames(df) <- names(object$results[[1]]$properties)
            for (index in 1:length(object$results)) { # todo auto type
              # col 1
              for (col_index in 1:length(colnames(df))) {
                tmp_element <- object_create_auto(object$results[[index]]$properties[[colnames(df)[col_index]]])
                df[index,colnames(df)[col_index]] <- paste0(notion_simplify(tmp_element), collapse = ", ")
              }
            }
            return(df)
          })

#########################################################
##
##  notion_title Class & Methods
##
#########################################################

#' The notion_title Class
#' @description In Notion database, there is a type of result, title.
#' 
#' @slot id data element id
#' @slot type data type
#' @slot text data content
#' @slot link url link
#' @slot annotations style properties
#' @slot plain_text data content without properties
#' @slot href url link
#' @name notion_title-class
#' @rdname notion_title-class
#' @concept object
#' @exportClass notion_title

notion_title <- setClass(
  Class = "notion_title",
  slots = list(
    id = "character",
    type = "character",
    text = "character",
    link = "character",
    annotations = "list",
    plain_text = "character",
    href = "character"
  )
)

#' Create notion_title Object
#' @description create a new object for title data
#' 
#' @importFrom methods new
#' @param list input data in list format
#' @return Returns a notion_title object
#' 
#' @export
#' @concept object

object_create_title <- function(list){
  # jadge
  if(list[["type"]] == "title"){
    # value
    id = list[["id"]]
    type = list[["type"]]
    ## Question: Which case the length of tilte list will be longer than 1?
    text = list[["title"]][[1]]$text$content
    if(is.null(list[["title"]][[1]]$text$link)){
      link = "NULL"
    }else{
      link = list[["title"]][[1]]$text$link
    }
    ## Quetion: More case will change.
    annotations = list(bold = list[["title"]][[1]]$annotations$bold,
                       italic = list[["title"]][[1]]$annotations$italic,
                       strikethrough = list[["title"]][[1]]$annotations$strikethrough,
                       underline = list[["title"]][[1]]$annotations$underline,
                       code = list[["title"]][[1]]$annotations$code,
                       color = list[["title"]][[1]]$annotations$color)
    plain_text = list[["title"]][[1]]$plain_text
    if(is.null(list[["title"]][[1]]$href)){
      href = "NULL"
    }else{
      href = list[["title"]][[1]]$href
    }
    object <- new("notion_title", id = id, type = type, text = text, link = link, annotations = annotations, plain_text = plain_text, href = href)
    return(object)
  }else{
    message("Not title data")
  }
}


#' show notion_title
#' @description show method for S4 class notion_title
#' 
#' @param object notion_title object
#' @importMethodsFrom methods show
#' @export

setMethod("show",
          "notion_title",
          function(object){
            cat(object@plain_text)
          }
)

#' simplify notion_* object generic method
#' @description for simplifing different notion object to same type output
#' 
#' @param object notion_* object
#' @rdname notion_simplify
#' @export

setGeneric("notion_simplify", function(object) {
  standardGeneric("notion_simplify")
})

#' Simplify notion_title
#' @description for simplifing notion_title object to simple type output
#' 
#' @param object notion_title object
#' @export

setMethod("notion_simplify",
          "notion_title",
          function(object){
            return(object@plain_text)
          })

#' jadge object type is notion_title
#' @description if type is notion_title, return TRUE
#'
#' @param object input data object
#' 
#' @export

is.notion_title <- function(object){
  if(object@type == "title"){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

#########################################################
##
##  notion_multi_select Class & Methods
##
#########################################################

#' The notion_multi_select Class
#' @description In Notion database, there is a type of result, multi select.
#' 
#' @slot id data element id
#' @slot type end time
#' @slot data data type
#' @name notion_multi_select-class
#' @rdname notion_multi_select-class
#' @concept object
#' @exportClass notion_multi_select

notion_multi_select <- setClass(
  Class = "notion_multi_select",
  slots = list(
    id = "character",
    type = "character",
    data = "data.frame"
  )
)

#' Create notion_multi_select Object
#' @description create a new object for multi select data
#' 
#' @importFrom methods new
#' @param list input data in list format
#' @return Returns a notion_multi_select object
#' 
#' @export
#' @concept object

object_create_multi_select <- function(list){
  # jadge
  if(list[["type"]] == "multi_select"){
    # value
    id = list[["id"]]
    type = list[["type"]]
    data = as.data.frame(t(matrix(unlist(list[["multi_select"]]),3)),stringsAsFactors = FALSE)
    colnames(data) <- c("id","name","color")
    object <- new("notion_multi_select", id = id, type = type, data = data)
    return(object)
  }else{
    message("Not muliti select data")
  }
}


#' show notion_multi_select
#' @description show method for S4 class notion_multi_select
#' 
#' @param object notion_multi_select object
#' @importMethodsFrom methods show
#' @export

setMethod("show",
          "notion_multi_select",
          function(object){
            cat(object@data$name)
          }
)

#' Simplify notion_multi_select
#' @description for simplifing notion_multi_select object to simple type output
#' 
#' @param object notion_multi_select object
#' @export

setMethod("notion_simplify",
          "notion_multi_select",
          function(object){
            return(object@data$name)
          })

#' jadge object type is notion_multi_select
#' @description if type is notion_multi_select, return TRUE
#'
#' @param object input data object
#' 
#' @export

is.notion_multi_select <- function(object){
  if(object@type == "multi_select"){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

#########################################################
##
##  notion_date Class & Methods
##
#########################################################

#' The notion_date Class
#' @description In Notion database, there is a type of result, date.
#' 
#' @slot id data element id
#' @slot type data type
#' @slot start start time
#' @slot end end time
#' @slot time_zone time zone
#' @name notion_date-class
#' @rdname notion_date-class
#' @concept object
#' @exportClass notion_date

notion_date <- setClass(
  Class = "notion_date",
  slots = list(
    id = "character",
    type = "character",
    start = "character",
    end = "character",
    time_zone = "character"
  )
)

#' Create notion_date Object
#' @description create a new object for date data
#' 
#' @importFrom methods new
#' @param list input data in list format
#' @return Returns a notion_date object
#' 
#' @export
#' @concept object

object_create_date <- function(list){
  # jadge
  if(list[["type"]] == "date"){
    # value
    id = list[["id"]]
    type = list[["type"]]
    start = list[["date"]]$start
    if(is.null(list[["date"]]$end)){
      end = "NULL"
    }else{
      end = list[["date"]]$end
    }
    if(is.null(list[["date"]]$time_zone)){
      time_zone = "NULL"
    }else{
      time_zone = list[["date"]]$time_zone
    }
    object <- new("notion_date", id = id, type = type, start = start, end = end, time_zone = time_zone)
    return(object)
  }else{
    message("Not date data")
  }
}


#' show notion_date
#' @description show method for S4 class notion_date
#' 
#' @param object notion_date object
#' @importMethodsFrom methods show
#' @export

setMethod("show",
          "notion_date",
          function(object){
            cat(object@start)
          }
)

#' Simplify notion_date
#' @description for simplifing notion_date object to simple type output
#' 
#' @param object notion_date object
#' @export

setMethod("notion_simplify",
          "notion_date",
          function(object){
            return(object@start)
          })

#' jadge object type is notion_date
#' @description if type is notion_date, return TRUE
#'
#' @param object input data object
#' 
#' @export

is.notion_date <- function(object){
  if(object@type == "date"){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

#' filter notion_date generic method
#' @description for filtering records in date region
#' 
#' @param object notion_date object
#' @param min min date
#' @param max max date
#' @rdname notion_filter
#' @export

setGeneric("notion_filter", function(object,min,max) {
  standardGeneric("notion_filter")
})

#' filter notion_date
#' @description for filtering records in date region
#' 
#' @importFrom lubridate ymd_hms
#' @importFrom lubridate ymd
#' @importFrom stringr str_detect
#' @param object notion_date object
#' @param min min date
#' @param max max date
#' @export

setMethod("notion_filter",
          "notion_date",
          function(object, min=today(),max=today()){
            min <- ymd(min)
            max <- ymd(max)
            if(is.notion_date(object)){
              if(str_detect(object@start,"[T+]")){
                start <- ymd_hms(object@start,tz = "Asia/Shanghai")
                start <- as.Date(start)
              }else{
                start <- ymd(object@start)
              }
              if(start >= min & start <= max){
                return(object)
              }else{
                return(NULL)
              }
            }else{
              
            }
          })


#########################################################
##
##  notion_rich_text Class & Methods
##
#########################################################

#' The notion_rich_text Class
#' @description In Notion database, there is a type of result, title.
#' 
#' @slot id data element id
#' @slot type data type
#' @slot text data content
#' @slot link url link
#' @slot annotations style properties
#' @slot plain_text data content without properties
#' @slot href url link
#' @name notion_rich_text-class
#' @rdname notion_rich_text-class
#' @concept object
#' @exportClass notion_rich_text

notion_rich_text <- setClass(
  Class = "notion_rich_text",
  slots = list(
    id = "character",
    type = "character",
    text = "character",
    link = "character",
    annotations = "list",
    plain_text = "character",
    href = "character"
  )
)

#' Create notion_rich_text Object
#' @description create a new object for rich_text data
#' 
#' @importFrom methods new
#' @param list input data in list format
#' @return Returns a notion_rich_text object
#' 
#' @export
#' @concept object

object_create_rich_text <- function(list){
  # jadge
  if(list[["type"]] == "rich_text"){
    # value
    id = list[["id"]]
    type = list[["type"]]
    ## Question: Which case the length of tilte list will be longer than 1?
    if(length(list[["rich_text"]]) == 0){
      text = "NULL"
      link = "NULL"
      annotations = list()
      plain_text = "NULL"
      href = "NULL"
    }else{
      text = list[["rich_text"]][[1]]$text$content
      if(is.null(list[["rich_text"]][[1]]$text$link)){
        link = "NULL"
      }else{
        link = list[["rich_text"]][[1]]$text$link$url
      }
      ## Quetion: More case will change.
      annotations = list(bold = list[["rich_text"]][[1]]$annotations$bold,
                         italic = list[["rich_text"]][[1]]$annotations$italic,
                         strikethrough = list[["rich_text"]][[1]]$annotations$strikethrough,
                         underline = list[["rich_text"]][[1]]$annotations$underline,
                         code = list[["rich_text"]][[1]]$annotations$code,
                         color = list[["rich_text"]][[1]]$annotations$color)
      plain_text = list[["rich_text"]][[1]]$plain_text
      if(is.null(list[["rich_text"]][[1]]$href)){
        href = "NULL"
      }else{
        href = list[["rich_text"]][[1]]$href
      }
    }
    object <- new("notion_rich_text", id = id, type = type, text = text, link = link, annotations = annotations, plain_text = plain_text, href = href)
    return(object)
  }else{
    message("Not rich_text data")
  }
}


#' show notion_rich_text
#' @description show method for S4 class notion_rich_text
#' 
#' @param object notion_rich_text object
#' @importMethodsFrom methods show
#' @export

setMethod("show",
          "notion_rich_text",
          function(object){
            cat(object@plain_text)
          }
)

#' Simplify notion_rich_text
#' @description for simplifing notion_rich_text object to simple type output
#' 
#' @param object notion_rich_text object
#' @export

setMethod("notion_simplify",
          "notion_rich_text",
          function(object){
            return(object@plain_text)
          })

#' jadge object type is notion_rich_text
#' @description if type is notion_rich_text, return TRUE
#'
#' @param object input data object
#' 
#' @export

is.notion_rich_text <- function(object){
  if(object@type == "rich_text"){
    return(TRUE)
  }else{
    return(FALSE)
  }
}