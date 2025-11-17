#'calculate the growth rate constant
#'
#'This equation originates from Armitage 2013
#'@param biota_mass (kg) the mass of the biota, greater than 0
#'@param water_temp (celsius) water temperature with a default of 25
#'@returns (1/days) a float or vector of floats corresponding to the growth rate constant
#'of the run
#'@examples
#'calc_growth_rate(0.1)
#'calc_growth_rate(0.1, 20)
calc_growth_rate <- function(biota_mass, water_temp = 25){

  if(biota_mass<=0){
    stop("The Biota mass is equal to 0 or less")
  }
  #growth rate constant
  growth_rate <- 0.00586*(1.113^(water_temp-20))*(1000*biota_mass)^(-0.2)

  return(growth_rate)

}
