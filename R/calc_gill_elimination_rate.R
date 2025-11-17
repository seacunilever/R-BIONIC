#' Calculate gill elimination constant rate
#'
#' The calculation is mainly from the uptake rate and distribution ratio of fish water
#' The distribution ratio tries to capture the ion trapping effect in the model
#' @param gill_uptake_rate (L/(kg days))  rate constant of uptake via gill ventilation
#' @param dfw_uptake_lysosome Fish-water distribution ratio(internal), recalculated to account for lysosomal sequestration
#' @param ratio_bio (unitless)speciation at biological pH. See \link[BIONICv3]{calc_ratio}
#' @param ratio_bulk (unitless)speciation at bulk pH. See \link[BIONICv3]{calc_ratio}
#' @param ion_type (unitless) the chemical's ionisation type which includes "A"(acid),
#' "B"(base), "N"(neutral) and "Q"(QAC)
#' @return (1/days) The gill elimination/clearance rate constant
calc_gill_elimination_rate <- function(gill_uptake_rate, dfw_uptake_lysosome, ratio_bio, ratio_bulk,
                                       ion_type, z_water_value = 1){

  #if it's a QAC then no ratio is used
  if (ion_type=="Q"){

    z_w_bio <- z_water_value
    z_w_bulk <- z_water_value
  }else{
    z_w_bio <- z_water_value*(1+ratio_bio)
    z_w_bulk <- z_water_value*(1+ratio_bulk)
  }

  if(dfw_uptake_lysosome*z_w_bio==0 || z_w_bulk==0){
    stop("Division by zero error")
  }

  dfw_i <- (dfw_uptake_lysosome*z_w_bio)/z_w_bulk

  gill_elimination_rate <- gill_uptake_rate/dfw_i

  return(gill_elimination_rate)
}
