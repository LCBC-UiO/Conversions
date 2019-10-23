
#' Calculate BMI
#' 
#' function to calculate the BMI
#' with the standard formulae: 
#' \eqn{BMI = weight / (height/100)^2 }
#' 
#' @param height height in centimeters
#' @param weight weight in kilograms
#'
#' @return numeric vector of BMI
#' @export
#'
#' @examples
#' calc_bmi(176, 72)
calc_bmi <- function(height, weight){
  weight/((height/100)^2)
}

#' Calculate BMI
#' 
#' function to calculate the BMI
#' with the non-standard formulae: 
#' \eqn{BMI = weight / (height/100)^2.5 }
#' which is suggested to provide a measurement that
#' it less dependent on height.
#' 
#' @inheritParams calc_bmi
#'
#' @return numeric vector of BMI
#' @export
#'
#' @examples
#' calc_bmi2(176, 72)
calc_bmi2 <- function(height, weight){
  weight/((height/100)^2.5)
}