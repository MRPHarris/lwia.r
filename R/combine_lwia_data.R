#' Combine lgr data
#'
#' @description Combine several parsed files into a single data frame for subsequent analysis.
#'
#' @param parsed_data an output from parse_lwia_files or trim_lwia_data
#' @param file_rownames logical TRUE/FALSE to include file names as ronwames
#'
#' @importFrom magrittr %>%
#' @importFrom tibble remove_rownames
#' @importFrom rlist list.rbind
#'
#' @export
#'

combine_lwia_data <- function(parsed_data,
                              file_rownames = FALSE){
  data_combined <- list.rbind(parsed_data)
  if(!file_rownames){
    data_combined <- data_combined %>% remove_rownames()
  }
}
