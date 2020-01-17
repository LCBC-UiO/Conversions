
#' Calculate the mean blood pressure
#' 
#' functions that given a data.frame
#' and columns selection in argument 'cols'
#' using tidy selectors, will calculate the mean
#' 
#'
#' @param data data frame
#' @param cols columns selected with tidy selectors
#' @param na.rm logical. Should missing values (including
#'  NaN) be omitted from the calculations?
#'
#' @return numeric vector with mean
#' @export
#' @family blood pressure functions
#' @importFrom dplyr mutate select
#' @examples
#'   dt <- data.frame(
#'   BloodPress_Diastolic_1 = c(80,32,66,NA),
#'   BloodPress_Diastolic_2 = c(58,45,NA,99),
#'   BloodPress_Systolic_1 = c(40,NA,80,120),
#'   BloodPress_Systolic_2 = c(NA, 65,45,100)
#'   )
#'   
#'   bloodpress_mean(dt, dplyr::contains("Diastolic"))
#'   bloodpress_mean(dt, dplyr::contains("Systolic"))
bloodpress_mean <- function(data, cols, na.rm = TRUE){
  tmp <- data %>% 
    dplyr::select({{cols}}) %>% 
    dplyr::mutate(mt = rowMeans(., na.rm = na.rm))
  tmp$mt
}

#' Calculate mean arterial pressure
#' 
#' Calculates the mean arterial pressure
#' based on diastolic and systolic blood pressure.
#' \eqn{MAP = (diastolic * 2)  + systolic / 3 } 
#' 
#'
#' @param diastolic diastolic blood pressure
#' @param systolic systolic blood pressure
#'
#' @return numeric vector of mean arterial pressure
#' @export
#' @family blood pressure functions
#' @examples
#' bloodpress_map(69, 40)
bloodpress_map <- function(diastolic, systolic){
  ((diastolic*2)+systolic)/3
}


if(getRversion() >= "2.15.1")  utils::globalVariables(c(""))
