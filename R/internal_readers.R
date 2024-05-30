# Internal file parsing functions

#' Parse processed files
#'
#' @description Read and format a processed lwia analysis file.
#'
#' @param filename_full A complete file path as a string.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#'
#' @noRd
#'

parse.processed <- function(filename_full){
  data <- read.delim(filename_full) %>%
    data.frame() %>%
    'colnames<-'(c("sample","density_H2O","proc_2H","proc_2H_sd","proc_18O","proc_18O_sd","proc_17O","proc_17O_sd","nb_metric","bb_metric","averaged_samples"))
  # if(isTRUE(rm_DI)){
  #   data <- data %>%
  #     filter(., !grepl('DI|DIS', sample))
  # }
  # if(isTRUE(rm_calib)){
  #   data <- data %>%
  #     filter(., !grepl('GRESP|SLAP2', sample))
  # }
  data
}
