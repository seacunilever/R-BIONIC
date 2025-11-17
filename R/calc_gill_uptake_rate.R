#' Calculate the uptake efficiency
#'
#' Calculates the uptake efficiency by looking at the transport of a chemical through the gill
#' both the charged and neutral forms of the chemical derived from Armitage BIONICv3 manual guide
#'
#' @param partition_list (unitless)A list of partition coefficients including membrane water partition
#' coefficient neutral and charged form
#' @param ratio_gill The ratio charged to neutral form at gill pH. See \link[BIONICv3]{calc_ratio}
#' @param ion_type The ionistation type which includes "A"(acid), "B"(Base), "N"(Neutral), "Q"(QAC)
#' @param speciation_gill_neut (unitless)  The fraction of the chemical in neutral form at gill pH. See \link[BIONICv3]{calc_speciation_neut}
#' @param speciation_gill_ion (unitless) The fraction of the chemical in charged form at gill pH
#' @param resistance_at_gill (unitless) The relative resistance of chemical transport through aqueous and organic
#'  lipid diffusion layers at the gills. Default at 1.85
#' @param resistance_within_gill (unitless) The relative resistance of chemical transport through aqueous and organic
#'  lipid diffusion layers within the gills. Default at 155.
#' @param z_water_value (unitless) Z-value for chemical in water(default is 1 from the aquivalence approach refrenced in Mackay's book chapter 2.7.5
#' @param gamma_eff (unitless) The extent to which the diffusivity of the charged form of the chemical in
#' organic phases is reduced compared to the neutral form of the chemical
#' @param beta_eff the potential for passive uptake of the chemical in any form via paracellular
#' routes defualted at 0.0001 in Armitage 2013.
#' @return (unitless) The chemical uptake efficiency which will be between 0 and 1
calc_gill_uptake_efficiency <- function(partition_list, ratio_gill, ion_type, speciation_gill_neut,
                                   speciation_gill_ion, resistance_at_gill = 1.85, resistance_within_gill = 155, z_water_value = 1,
                                   gamma_eff = NA, beta_eff = 0.0001){

  #gets the gamma efficiency from the ion_type
  if (is.na(gamma_eff)){
    if (ion_type == "A"){
      gamma_eff <- 10000
    }else if(ion_type == "B"){
      gamma_eff <- 300000
    }else{
      gamma_eff <- 100
    }
  }

  if(resistance_within_gill==0|gamma_eff==0){
    stop("Either the resistance_within_gill or gamma_eff are 0 which would lead to a division by 0")
  }

  # Changes the value depending if the ion type is a QAC
  if (ion_type == "Q"){
    z_w_gill <- z_water_value
    z_wi_gill <- z_water_value
    z_on <- 0

  }
  else{
    z_w_gill <- z_water_value*(1 + ratio_gill)
    z_wi_gill <- z_water_value*ratio_gill
    z_on <- partition_list$KMWN*z_water_value
  }

  z_oi <- partition_list$KMWI*z_wi_gill


  neutral_transport <- (z_on*speciation_gill_neut)/resistance_within_gill
  ion_transport <- (z_oi*speciation_gill_ion)/(resistance_within_gill*gamma_eff)

  #removes division by 0
  if(neutral_transport+ion_transport==0){
    stop("The neutral and ion transport sum is equal to 0 which is not possible due to division by 0")
  }

  transport <- z_w_gill/(neutral_transport+ion_transport)

  if(resistance_at_gill+transport==0){
    stop("The sum of the transport is equal to 0 which is not possible due to division by 0")
  }

  gill_uptake_efficiency <- (resistance_at_gill + transport)^(-1) + beta_eff

  return(gill_uptake_efficiency)

}

#' Calculate the oxygen concentration
#'
#' Calculate the oxygen concentration using the water temperature and saturation. The equation is referenced
#' in Norstorm 1976
#' @param water_temp (Celsius)The water temperature
#' @param saturation (unitless) The percent saturation of the water. Default is 0.9
#' @return (mg/L) The oxygen concentration in the water
calc_oxygen_conc_water <- function(water_temp = 25, saturation = 0.9){

  water_quadratic <- 14.45 - 0.413*water_temp + 0.00556*(water_temp^2)
  oxygen_conc_water <- saturation*water_quadratic

  return(oxygen_conc_water)
}

#' Calculate the metabolic oxygen
#'
#' @param biota_mass (kg)the wet weight of the fish
#' @param water_temp (Celsius)The water temperature
#' @return (mg/day) The metabolic oxygen requirements of the fish
calc_metabolic_oxygen <- function(biota_mass, water_temp = 25){

  if (biota_mass<=0){
    stop("Biota mass has to greater than 0")
  }
  log_metabolic_oxygen <- 2.8 + 0.786*log10(biota_mass)+0.017*water_temp

  metabolic_oxygen <- 10^(log_metabolic_oxygen)

  return(metabolic_oxygen)
}

#' Calculate the gill ventilation rate
#'
#' @param oxygen_conc_water (mg/L) The oxygen concentration in the water
#' @param metabolic_oxygen (mg/day)the metabolic oxygen requirements of the fish
#' @param eff_oxy_transfer (unitless)the efficiency of oxygen transfer across the gills
#' @return (L/days) The gill ventilation rate
calc_gill_vent_rate <- function(oxygen_conc_water, metabolic_oxygen, eff_oxy_transfer = 0.65){

  if(oxygen_conc_water==0 || eff_oxy_transfer==0){
    stop("Oxygen concentration or the the efficiency of oxygen cannot be 0 due to division by 0")
  }
  gill_Vent_rate <- metabolic_oxygen/(oxygen_conc_water*eff_oxy_transfer)

  return(gill_Vent_rate)
}

#' Calculate the uptake rate
#'
#' @param biota_mass (kg)The wet weight of the biota
#' @param gill_vent_rate (L/days) The gill ventilation rate. See \link[BIONICv3]{calc_gill_vent_rate}
#' @param gill_uptake_efficiency (unitless)The chemical uptake efficiency at the gill. See \link[BIONICv3]{calc_gill_uptake_efficiency}.
#' @return (L/(kg days)) The rate constant of uptake via the gill ventilation
calc_gill_uptake_rate <- function(biota_mass, gill_vent_rate, gill_uptake_efficiency){

  if(biota_mass<=0){
    stop("masss of biota cannot be less than or equal to 0")
  }

  gill_uptake_rate <- gill_uptake_efficiency*(gill_vent_rate/biota_mass)

  return(gill_uptake_rate)
}
