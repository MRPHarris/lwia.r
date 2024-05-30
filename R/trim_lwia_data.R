#' Trim one or more parsed LWIA data files
#'
#' @description Trim one or more LWIA data files parsed into R by removing unwanted measurement types. This performs a lapply
#' wrap around the internal function clean_data.
#'
#' @param remove_DI NULL or character vector with DI water sample/control/standard strings.
#' @param remove_externals NULL or character vector with external calibration sample/control/standard strings.
#' @param remove_internals NULL or character vector with internal calibration sample/control/standard strings.
#'
#'

trim_lwia_data <- function(parsed_data,
                           remove_DI = c('DI','DIS'),
                           remove_externals = c('VSMOW2','VMSOW','GRESP','SLAP','SLAP2'),
                           remove_internals = NULL){

  # The input will be a nested list containing individual data columns.
  # Lapply this bad boy
  # testing vars
  # parsed_data = parse_lwia_files(target = "E:/mh work live 2024-05-07/Analysis/PH22 isotopes/Processed data/Round 1/", type = "Processed", rc = 0)
  # remove_DI = c('DI','DIS')
  # remove_externals = c('VSMOW2','VMSOW','GRESP','SLAP','SLAP2')
  # remove_internals = NULL
  parsed_data_cleaned <- lapply(parsed_data, clean_data, DI = remove_DI, externals = remove_externals, internals = remove_internals)
  return(parsed_data_cleaned)
}
