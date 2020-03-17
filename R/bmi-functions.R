
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
bmi_calc <- function(height, weight, unit = list(height = "cm", 
                                                 weight = "kg")){
  
  corrected <- bmi_unit(height, weight, unit)
  
  corrected$weight/corrected$height^2
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
bmi_calc2 <- function(height, weight, unit = list(height = "cm", 
                                                  weight = "kg")){
  
  corrected <- bmi_unit(height, weight, unit)
  
  corrected$weight/corrected$height^2.5
}

bmi_unit <- function(height, weight, unit = list(height = "cm", 
                                                 weight = "kg")){
  
  if(missing(height)) stop("height is missing", call.=FALSE)
  if(missing(weight)) stop("weight is missing", call.=FALSE)
  
  unit_height <- match.arg(unit$height, c("m", "cm"))
  unit_weight <- match.arg(unit$weight, c("g", "kg"))
  
  if(unit_height == "cm") height = height/100
  if(unit_weight == "g")  weight = weight/100
  
  return(list(height = height, weight = weight))
}