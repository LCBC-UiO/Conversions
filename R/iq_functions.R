#' Convert raw IQ measures to T or scaled
#' 
#' @param data data.frame with raw data
#'
#' @param type string indicating type of conversion T [default] or scaled
#' @param battery from which test battery 'WASI'[default] or 'WPPSI'
#' @param subtest which subtest of battery
#' @param table Table with conversions, either directly as data.frame or path to file
#' @param keep unused option as of now
#' @param ... extra options to rio::import for table import
#'
#' @importFrom dplyr filter mutate select contains
#' @importFrom purrr map map2
#' @importFrom rio import
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr separate unnest
#' @importFrom magrittr '%>%'

convert_rawIQ = function(data, type = "T", battery = "WASI", subtest = "Vocab", 
                         table = NULL, keep = F, ...){
  
  stopifnot(!is.null(table))
  
  # If no table had been provided, file should be provided
  if(is.character(table)){
    table = rio::import(file=table, ...)
  }
  
  # Calculate Age in decimals from Age with month
  TABLE = table %>% 
    dplyr::filter(Subtest %in% subtest) %>% 
    tidyr::separate(Age, c("Age", "DEC"), sep=":", convert=T) %>% 
    dplyr::mutate(DEC = DEC/12, 
                  Age = ifelse(is.na(DEC), Age, Age + DEC)) %>% 
    dplyr::select(-DEC)
  
  # Grab the columns necessary and make some placeholders
  tmp = data %>% 
    dplyr::select(Age, dplyr::contains(paste(battery, subtest, "Raw", sep = "_"))) %>% 
    dplyr::mutate(ROW = NA,NEW=NA, subtest=subtest)
  names(tmp)[2:3] = c("RAW",type)
  
  # Get the converted scores
  tmp = tmp %>% 
    tibble::rownames_to_column() %>% 
    dplyr::mutate(ROW = purrr::map(Age, ~.-TABLE$Age)) %>% 
    dplyr::mutate(ROW = purrr::map(ROW, ~which.min(.[.>=0]))) %>% 
    dplyr::mutate(ROW = purrr::map(ROW, ~ifelse(is_empty(.), NA, .))) %>% 
    tidyr::unnest() %>% 
    dplyr::mutate(NEW = purrr::map2(.x = ROW, .y = RAW, 
                                    ~.y - TABLE[.x, 3:ncol(TABLE)]) ) %>% 
    dplyr::mutate(NEW = purrr::map(NEW, ~names(.)[which.min(.[.>=0])])) %>% 
    dplyr::mutate(NEW = purrr::map(NEW, ~ifelse(is_empty(.), NA, .))) %>% 
    tidyr::unnest() %>% 
    dplyr::rename(OLD = type) %>% 
    dplyr::mutate(NEW = ifelse(is.na(NEW), OLD, NEW))
  
  # For easier working, copy the data
  data2 = data
  
  # Set the name for the new column
  nm = paste(battery, subtest,type, sep="_")
  nm = ifelse(keep, paste(nm, "New", sep="_"), nm)
  
  # Add the new column to the entire data
  data2[,nm] = as.numeric(tmp$NEW)
  
  return(data2)
}


wasi.conversion = function(IN, wasi.tb) {
  
  #set t-values outside allowed range to 999
  Int = IN %>% 
    ifelse(. < min(wasi.tb[,1]) | . > max(wasi.tb[-length(wasi.tb[,1]),1]), 
           999, .) 
  
  #Get the values from the wasi table at the extrected indeces
  wasi.tb[match(Int, wasi.tb[,1]),2]
}


if(getRversion() >= "2.15.1")  utils::globalVariables(c("Subtest", "Age","BloodPress_MapMean", 
                                                        "DEC", "ROW",
                                                        "RAW","NEW",
                                                        "OLD", "."))