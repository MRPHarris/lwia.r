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
  data
}

#' Parse detailed files
#'
#' @description Read and format a detailed lwia analysis file. Option to output just the per-injection data, the diagnostic information, or both.
#'
#' @param filename_full a full path to the target detailed file.
#' @param content one of "data", "diagnostics", or "all". Returns the per-injection data as a dataframe, the diagnostic info as a multi-element list, or both (each as an item within a two-element list), respectively.
#'
#' @importFrom magrittr %>%
#'
#' @noRd
#'
parse.detailed <- function(filename_full,
                           content = "all"){
  if(content == "data"){
    dataL <- readLines(filename_full) #%>%
    ind <- grep("Injection Number",dataL) %>% as.numeric()
    data <- read.delim(filename_full, skip = ind-1, header = T) %>%
      data.frame()
    return(data)
  } else if(content == "diagnostics"){
    # Get and trim diagnostic information
    diagnostics <- detailed.diag.reader(filename_full)
    return(diagnostics)
  } else if(content == "all"){
    # diag
    diagnostics <- detailed.diag.reader(filename_full)
    # data
    dataL <- readLines(filename_full) #%>%
    ind <- grep("Injection Number",dataL) %>% as.numeric()
    data <- read.delim(filename_full, skip = ind-1, header = T) %>%
      data.frame()
    # combine as list
    dtl_list <- list(diagnostics,data) %>% 'names<-'(c('diagnostics','data'))
    return(dtl_list)
  } else {
    stop(paste0("parse.detailed content param: '",content,"' not recognised."))
  }
}

#' Parse detailed file diagnostic info
#'
#' @description Parse the diagnostic info within a detailed file. Parses the file line-by-line, and assumes line numbers are constant between files.
#'
#' @param filename_full a full path to the target detailed file.
#'
#' @importFrom magrittr %>%
#'
#' @noRd
#'
detailed.diag.reader = function(filename_full){
  diag <- read.delim(filename_full, header = T, sep = "\n")
  diag_trim <- diag[1:as.numeric(grep("Injection Number",diag[,1])-1),] %>% as.matrix()
  #### This could be wrapped as a e.g. detailed.diag.constructor
  diag_list <- vector('list', length = 14)
  # Init elements
  diag_list[[6]] <- vector('list', length = 3) # analysis
  diag_list[[7]] <- vector('list', length = 19) # rej filters
  diag_list[[8]] <- vector('list', length = 15) # filter settings
  diag_list[[9]] <- vector('list', length = 2) # user settings
  diag_list[[10]] <- vector('list', length = 4) # injected volume settings
  diag_list[[11]] <- vector('list', length = 7) # control settings
  diag_list[[12]] <- vector('list', length = 3) # replicate averaging settings
  diag_list[[13]] <- vector('list', length = 9) # misc settings
  diag_list[[14]] <- vector('list', length = 1) # standards
  # Names are acquired by splitting certain strings at the colon.
  initial_headers <- unlist(lapply(strsplit(diag_trim[1:5],"[:]"),"[[",1))
  list_headers <- unlist(lapply(strsplit(diag_trim[c(6,10,30,46,49,54,62,66,76)],"[:]"),"[[",1))
  names(diag_list) <- c(initial_headers,list_headers)
  ## Initial rows
  # contents
  diag_list[1:4] <- split.colon(diag_trim[1:4,], get = 2, forcesb = T)
  diag_list[5] <- paste(unlist(lapply(strsplit(diag_trim[5],"[:]"),"[",c(2,3,4))), collapse = ":") %>% trimws()
  ## ANALYSIS
  # contents
  analysis_contents <- diag_trim[7:9,] %>%
    set_sb(c(1,3),split.equals(diag_trim[c(7,9),],2)) %>%
    set_mb(2,split.colon(diag_trim[8,],2))
  diag_list[[6]][c(1:3)] <- analysis_contents
  # names
  analysis_names <- diag_trim[7:9,] %>%
    set_sb(c(1,3),split.equals(diag_trim[c(7,9),])) %>%
    set_mb(2,split.colon(diag_trim[8,]))# %>%
  # set(3,split.equals(diag_trim[9,]))
  names(diag_list[[6]]) <- analysis_names
  ## Rej filters
  # contents
  rejection_contents <- diag_trim[11:29,] %>%
    set_sb(.,split.equals(diag_trim[c(11:29),],2)) #%>%
  # set_mb(2,split.colon(diag_trim[8,],2))
  diag_list[[7]][c(1:19)] <- rejection_contents
  # names
  rejection_names <- diag_trim[11:29,] %>%
    set_sb(.,split.equals(diag_trim[c(11:29),])) #%>%
  # set_mb(2,split.colon(diag_trim[8,]))# %>%
  # set(3,split.equals(diag_trim[9,]))
  names(diag_list[[7]]) <- rejection_names

  ## Warning/rej filters
  filter_contents <- diag_trim[31:45,] %>%
    set_sb(.,split.equals(diag_trim[c(31:45),],2)) #%>%
  # set_mb(2,split.colon(diag_trim[8,],2))
  diag_list[[8]][c(1:15)] <- filter_contents
  # names
  filter_names <- diag_trim[31:45,] %>%
    set_sb(.,split.equals(diag_trim[c(31:45),])) #%>%
  # set_mb(2,split.colon(diag_trim[8,]))# %>%
  # set(3,split.equals(diag_trim[9,]))
  names(diag_list[[8]]) <- filter_names
  ## User settings
  # content
  stmp <- split.colon(diag_trim[c(47),],2)
  path_str <- unlist(lapply(strsplit(diag_trim[c(48),],"[:]"),tail,2)) %>%
    trimws() %>% paste(collapse = ":")
  user_contents <- c(stmp,path_str)
  diag_list[[9]][c(1,2)] <- user_contents
  # names
  user_names <- diag_trim[c(47,48),] %>%
    set_sb(.,split.colon(diag_trim[c(47,48),]))
  names(diag_list[[9]]) <- user_names
  ## Injected vol
  inj_contents <- diag_trim[50:53,] %>%
    set_sb(.,split.equals(diag_trim[c(50:53),],2)) #%>%
  # set_mb(2,split.colon(diag_trim[8,],2))
  diag_list[[10]][c(1:4)] <- inj_contents
  # names
  inj_names <- diag_trim[50:53,] %>%
    set_sb(.,split.equals(diag_trim[c(50:53),])) #%>%
  # set_mb(2,split.colon(diag_trim[8,]))# %>%
  # set(3,split.equals(diag_trim[9,]))
  names(diag_list[[10]]) <- inj_names
  ## Internal controls
  cntrl_contents <- diag_trim[55:61,] %>%
    set_mb(1,NA) %>%
    set_sb(c(2:7),split.equals(diag_trim[c(56:61),],2)) #%>%
  # set_mb(2,split.colon(diag_trim[8,],2))
  diag_list[[11]][c(1:7)] <- cntrl_contents
  # names
  cntrl_names <- diag_trim[55:61,] %>%
    set_sb(.,split.equals(diag_trim[c(55:61),])) #%>%
  # set_mb(2,split.colon(diag_trim[8,]))# %>%
  # set(3,split.equals(diag_trim[9,]))
  names(diag_list[[11]]) <- cntrl_names
  ## repavg settings
  repavg_contents <- diag_trim[63:65,] %>%
    set_sb(.,split.equals(diag_trim[c(63:65),],2)) #%>%
  # set_mb(2,split.colon(diag_trim[8,],2))
  diag_list[[12]][c(1:3)] <- repavg_contents
  # names
  cntrl_names <- diag_trim[63:65,] %>%
    set_sb(.,split.equals(diag_trim[c(63:65),])) #%>%
  # set_mb(2,split.colon(diag_trim[8,]))# %>%
  # set(3,split.equals(diag_trim[9,]))
  names(diag_list[[12]]) <- cntrl_names
  ## misc settings
  repavg_contents <- diag_trim[67:75,] %>%
    set_sb(c(1,2),split.colon(diag_trim[c(67,68),],2)) %>%
    set_sb(c(3:8),split.equals(diag_trim[c(69:74),],2)) %>%
    set_sb(c(9),unlist(lapply(strsplit(diag_trim[c(75),],"[:]"),tail,2)) %>%
             trimws() %>% paste(collapse = ":"))
  diag_list[[13]][c(1:9)] <- repavg_contents
  # names
  cntrl_names <- diag_trim[67:75,] %>%
    set_sb(c(1,2),split.colon(diag_trim[c(67,68),],1)) %>%
    set_sb(c(3:8),split.equals(diag_trim[c(69:74),],1)) %>%
    set_sb(c(9),split.colon(diag_trim[c(75),],1))
  # set_mb(2,split.colon(diag_trim[8,]))# %>%
  # set(3,split.equals(diag_trim[9,]))
  names(diag_list[[13]]) <- cntrl_names
  ## Standards
  diag_list[[14]] <- read.table(textConnection(diag_trim[(grep("Standards Used:",diag[,1])+1):nrow(diag_trim),]), header = T)
  diag_list
}

