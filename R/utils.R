# Utility functions (internal)

#' trim a filepath
#'
#' @description Trim the path from a filename or folder. Borrowed from the SampleQueue package
#'
#' @param filenames full paths to one or more files
#'
#' @noRd
#'
trim_path <- function(filenames){
  if(length(filenames) > 1){
    it_list <- vector(mode = "list", length = length(filenames))
    trimmed_filenames <- vector(mode = "character", length = length(filenames))
    for(f in seq_along(filenames)){
      filename_trimmed <- unlist(strsplit(filenames[f],"/"))[length(unlist(strsplit(filenames[f],"/")))]
      trimmed_filenames[f] <- filename_trimmed
    }
    trimmed_filenames
  } else if(length(filenames) == 1){
    filename <- filenames
    filename_trimmed <- unlist(strsplit(filename,"/"))[length(unlist(strsplit(filename,"/")))]
    filename_trimmed
  } else{
    message("Empty object; no path to trim.")
  }
}

#' Clean an individual parsed LWIA file
#'
#' @description a single-dataframe cleaning function, to be wrapped into a lapply within trim_lwia_data()
#'
#' @importFrom dplyr filter
#'
#' @noRd
#'
clean_data <- function(data,
                       DI = NULL,
                       externals = NULL,
                       internals = NULL){
  # DI
  if(!is.null(DI)){
    data <- data %>%
      filter(., !grepl(paste(DI,collapse = "|"), sample))
  }
  # externals
  if(!is.null(externals)){
    data <- data %>%
      filter(., !grepl(paste(externals,collapse = "|"), sample))
  }
  # internals
  if(!is.null(internals)){
    data <- data %>%
      filter(., !grepl(paste(internals,collapse = "|"), sample))
  }
  # return
  data
}

#' Split equals
#'
#' @description Split a string at an equals sign, and grab a specified element from the split.
#'
#'@param string the target string
#' @param get numeric; which element to get. passed to lapply.
#' @param trim_ws TRUE/FALSE to trim whitespace.
#'
#' @importFrom magrittr %>%
#'
#' @noRd
#'
split.equals <- function(string, get = 1, trim_ws = T, collapse = T){
  if(length(get) == 1){
    newstr <- unlist(lapply(strsplit(string,"[=]"),"[[",get))
  } else {
    # Single brackets
    newstr <- unlist(lapply(strsplit(string,"[=]"),"[",get))
  }
  if(collapse){
    newstr <- paste(newstr)
  }
  if(trim_ws){
    newstr <- newstr %>% trimws()
  }
  newstr
}

#' Split colon
#'
#' @description Split a string at a colon, and grab a specified element from the split.
#'
#' @param string the target string
#' @param get numeric; which element to get. passed to lapply.
#' @param trim_ws TRUE/FALSE to trim whitespace.
#'
#' @importFrom magrittr %>%
#'
#' @noRd
#'
split.colon <- function(string, get = 1, trim_ws = T, collapse = T, forcesb = F){
  if(length(get) == 1){
    if(forcesb){
      newstr <- unlist(lapply(strsplit(string,"[:]"),"[",get))
    } else {
      newstr <- unlist(lapply(strsplit(string,"[:]"),"[[",get))
    }
  } else {
    # Single brackets
    newstr <- unlist(lapply(strsplit(string,"[:]"),"[",get))
  }
  if(collapse){
    newstr <- paste(newstr)
  }
  if(trim_ws){
    newstr <- newstr %>% trimws()
  }
  newstr
}

#' Set elements in pipe with two brackets
#'
#' @description Streamlined indexing of list elements in pipe, from https://www.r-bloggers.com/2020/02/get-and-set-list-elements-with-magrittr/
#'
#' @param string the target string
#'
#' @noRd
#'
set_mb <- .Primitive("[[<-")

#' Set elements in pipe with single bracket
#'
#' @description Streamlined indexing of list elements in pipe, from https://www.r-bloggers.com/2020/02/get-and-set-list-elements-with-magrittr/
#'
#' @param string the target string
#'
#' @noRd
#'
set_sb <- .Primitive("[<-")
