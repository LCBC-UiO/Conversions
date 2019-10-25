
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
#' @family bmi functions
#' @examples
#' bmi_calc(176, 72)
bmi_calc <- function(height, weight){
  weight/((height/100)^2)
}

#' Calculate BMI
#'
#' function to calculate the BMI with the non-standard formulae: \eqn{BMI =
#' weight / (height/100)^(2.5) } which is suggested to provide a measurement that
#' it less dependent on height.
#'
#' @inheritParams bmi_calc
#'
#' @return numeric vector of BMI
#' @export
#' @family bmi functions
#' @examples
#' bmi_calc2(176, 72)
bmi_calc2 <- function(height, weight){
  weight/((height/100)^2.5)
}