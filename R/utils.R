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
