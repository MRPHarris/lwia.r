#' Get a standards library
#'
#' @description Find a LGR analyser standards library file
#'
#' @param directory a folder to search inside
#' @param name A character string to use for the hunt.
#'
#' @importFrom R.utils findFiles
#'
#' @export

get_standards_library = function(directory,
                                 name = "LGRLibrary"){
  # Get text
  txt <-  readLines(findFiles(pattern=name, paths=directory, recursive = T), warn = F)
  txt <- txt[3:length(txt)] # trim first lines
  txt <- gsub("(\t|\t)+","\t",txt) # combine adjacent tabs
  libfile <- read.table(text = txt, fill = T, sep = "\t", header = T) #%>%# convert to table
  # 'colnames<-'(gsub(".", " ", colnames(.), fixed=TRUE))
  libfile
}
