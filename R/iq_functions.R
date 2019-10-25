#' Convert raw IQ scores to scaled or T
#'
#' Will convert raw IQ scores from subtests into T or scaled values according to
#' a conversion table
#'
#' @param x raw iq score
#' @param age age in decimals
#' @param iq_table table with converions
#' 
#' @return numeric vector of converted IQ scores
#' @export
#' @family iq-functions
#' @examples
#' \dontrun{
#' t <- iq_table(".iq_table_subtest.tsv", "Vocab", header=TRUE)
#' iq_raw2score(31, 22, t)
#' iq_raw2score(x = c(33, 34, NA, 34), age=c(15.5, 20, 20, NA))
#' }
iq_raw2score <- function(x, age, iq_table){
  purrr::map2_dbl(x, age, ~ iq_get(.x, .y, iq_table))
}

#' Adjust WPPSI components to two subtest
#' 
#' WPPSI requires 3 or more subtests for 
#' verbal and performance IQ. There is an
#' adjustment that may be made for it to 
#' approximate using two subtests. This function 
#' applies this adjustment.
#'
#' @param scaled1,scaled2 scaled score from subtest
#'
#' @return scaled verbal/performance iq
#' @export
#'
#' @examples
#' iq_wppsi_adjust(c(10, 14), c(14, 16))
iq_wppsi_adjust <- function(scaled1, scaled2){
  ceiling((scaled1 + scaled2)*3/2)
}

#' Calculate full scale IQ from WPPSI
#' verbal and performance IQ
#'
#' @param verbal_iq unscaled verbal IQ
#' @param performance_iq unscaled performance IQ
#'
#' @return vector of full scale iq
#' @export
#'
#' @examples
#' iq_wppsi_fs(89, 96)
iq_wppsi_fs <- function(verbal_iq, performance_iq){
  (verbal_iq + performance_iq)/2
}

#' Import IQ conversion table
#' 
#' Import a punched version of the IQ
#' conversion table, for scaling raw
#' scores to norm or T-scores
#'
#' @param table path or data.frame with conversion data
#' @param subtest character vector indicating which subtest
#' @param ... arguments to \code{rio::import}
#'
#' @return long tibble of the wanted conversion table
#' @export
#' @family iq-functions
#' @importFrom dplyr mutate select filter as_tibble
#' @importFrom tidyr gather separate
#' @importFrom magrittr '%>%'
#' @examples
#' \dontrun{
#' iq_table("tables/wasi_table.tsv", "vocabulary")
#' }
iq_table <- function(table = NULL, subtest = NULL, ...){
  stopifnot(!is.null(table))
  
  # If no table had been provided, file should be provided
  if(is.character(table)){
    table = rio::import(file=table, ...)
  }
  
  # Make sure subtest matches possible subtests
  # also does partial matching as long as unique
  subtest <- match.arg(subtest, unique(table$Subtest))
  
  # Calculate Age in decimals from Age with month
  suppressWarnings(
    table %>% 
      dplyr::filter(Subtest %in% subtest) %>% 
      tidyr::separate(Age, c("Age", "DEC"), sep=":", convert=T) %>% 
      dplyr::mutate(DEC = DEC/12, 
                    Age = ifelse(is.na(DEC), Age, Age + DEC)) %>% 
      dplyr::select(-DEC) %>% 
      tidyr::gather(score, raw_score, -1:-2) %>% 
      dplyr::mutate(score = as.numeric(score)) %>% 
      dplyr::filter(!is.na(raw_score), !is.na(score)) %>% 
      dplyr::as_tibble()
  )
}

iq_get <- function(x, age, iq_table){
  
  tmp <- filter(iq_table, 
                raw_score == x,
                Age == min(iq_table$Age[iq_table$Age>=age])
  )
  
  ifelse(nrow(tmp)>0,tmp$score, NA_integer_)
}


#' Convert T-score to IQ
#' 
#' This function converts iq T-scores
#' to IQ, using the conversion table provided.
#' The conversion table provided, and the columns
#' in the data selected through the 'cols' argument
#' must correspond regarding the test battery used,
#' and the number of subtests provided for the IQ.
#' If providing 2 WASI subtests scores, the conversion
#' table must be for the conversion of two subtests
#' to fullscale IQ. For WPPSI, you must select
#' columns with verbal and performance IQ scaled
#' to calculate the unscaled verbal and performane IQ.
#' For WPPSI fullscale IQ, apply the \code{iq_wppsi_fs}
#' function, using the two unscaled verbal and 
#' performance IQs.
#'
#' @param data data.frame 
#' @param cols columns in the data frame with necessary
#' data
#' @param conversion_table table with conversion, first column
#' being the score to convert from, second score to 
#' convert to
#'
#' @return numeric vector of IQ scores
#' @export
#'
#' @examples
#' \dontrun{
#' ##
#' } 
iq_t2iq <- function(data, 
                    cols = NULL, 
                    conversion_table = NULL){
  
  if(is.null(conversion_table)){
    stop("Need 'conversion_table'", call.=FALSE)
  }else if(is.character(conversion_table)){
    conversion_table = rio::import_list(file)
  }
  
  tmp <- select(data, {{cols}})
  tmp <- rowSums(tmp, na.rm = FALSE)
  
  convert_t2iq(tmp, conversion_table)
}


convert_t2iq = function(x, conversion_table) {
  
  # set t-values outside allowed range to 999
  x = ifelse(x < min(conversion_table[,1]) | x > max(conversion_table[,1]), 
             999, x) 
  
  if(any(x == 999)){
    rr <- paste(which(x == 999), collapse=", ")
    warning(paste0("Some values are outside the allowed range. Row number(s):\t", rr))
  }
  
  # Get the values from the wasi table at the extracted indeces
  conversion_table[match(x, conversion_table[,1]),2]
}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("raw_score", "score", "DEC",
                                                        "Age", ".", "Subtest"))