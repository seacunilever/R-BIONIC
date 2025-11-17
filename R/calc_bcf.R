#'calculate the BCF
#'
#'The Bio Concentration Factor(BCF) is defined as the ratio the concentration of
#'a chemical in an organism to the concentration of the chemical in the
#'surrounding environment.In this equation it assumes steady state which therefore
#'uses the constant rates to calculate the BCF. This equation is referenced in Arnot 2004
#'
#'@param gill_uptake_rate (L/kg/day) The gill uptake rate constant. See \link[BIONICv3]{calc_gill_uptake_rate.Rd}
#'@param gill_elimination_rate (1/day) The gill elimination rate constant. See \link[BIONICv3]{calc_gill_elimination_rate}
#'@param growth_rate (1/day) The growth rate constant. See \link[BIONICv3]{calc_growth_rate}
#'@param fecal_egestion_rate (1/day) The fecal egestion rate constant. See \link[BIONICv3]{calc_fecal_egestion_rate}
#'@param biotransformation_rate (1/day) The biotransformation clearance rate. See \link[BIONICv3]{calc_biotransformation_rate}
#'@param vol_frac_list (unitless) A list of volume fractions used in the model, for this only the water volume fraction is used
#'@returns (L/kg) The BCF value is outputed
#'@examples
#'
#'vol_frac_list <- list(water=0.2, sl=0.3, sa=0.2, pw=0.3)
#'BIONICv3::calc_bcf(1, 0.1, 0.1, 0.1, 0.1, vol_frac_list)
calc_bcf <- function(gill_uptake_rate, gill_elimination_rate, growth_rate,
                     fecal_egestion_rate, biotransformation_rate, vol_frac_list){

  sum_elimns <- gill_elimination_rate + growth_rate + fecal_egestion_rate + biotransformation_rate

  if (sum_elimns==0){
    stop("Division by 0 error")
  }
  bcf <- vol_frac_list$water + gill_uptake_rate/sum_elimns

  return(bcf)
}
