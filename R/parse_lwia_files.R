#' Load LGR LWIA post-processing files into the workspace.
#'
#' @description Read and parse the two file types generated when saving processed data from the LWIA post-processing software
#'
#' @param target A complete folder or file path as a string.
#' @param type one of either "Processed" or "Detailed". The type of file to parse.
#' @param rc used by the function to tell if it is recursing (0 for no, 1 for yes).
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr mutate_at
#' @importFrom dplyr vars
#' @importFrom dplyr filter
#' @importFrom utils data
#' @importFrom tibble column_to_rownames
#'
#' @export
#'

parse_lwia_files <- function(target,
                             type = "Processed",
                             rc = 0){
  # Detect if file or directory
  if(dir.exists(target)){
    ## it's a directory. List and recurse.
    ## Type check.
    filenames <- list.files(target) # No full names for now
    filenames <- filenames[grep(type,filenames)]
    filenames_full <- paste0(target,filenames)
    ## Recursive iteration
    fmt_list <- vector('list', length = length(filenames_full))
    for(f in seq_along(fmt_list)){
      # Giving this recursion thing a go
      # f = 1
      fmt_list[[f]] <- parse_lwia_files(target = filenames_full[[f]],
                                        type = type,
                                        rc = 1)
      names(fmt_list) <- filenames
    }
    return(fmt_list)
  } else {
    # It's a file. Import and process.
    if(!isTRUE(grepl(type,trim_path(target)))){
      stop("Target is either not the correct type, or does not specify the type in its filename.")
    }
    if(type == "Processed"){
      data <- parse.processed(target)
    } else if(type == "Detailed"){
      data <- parse.detailed(target, content = "all")
    } else {
      stop("Type: ",type," not supported.")
    }
    if(rc == 0){
      # recursion
      datalist <- vector('list',1) %>%
        'names<-'(trim_path(target))
      datalist[[1]] <- data
      datalist
    } else {
      data
    }
  }
}

