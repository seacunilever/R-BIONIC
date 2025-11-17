#' Calculate intrinsic in vivo clearance
#'
#' Calculates the in vivo clearance either if the reaction rate is available or
#' instead if the intrinsic in vitro clearance is available it is used instead.
#' If neither are available, an error is will occur
#' @param reaction_rate (1/hour) Reaction rate
#' @param assay_s9 (mg/ml assay) concentration of S9 which is defaulted as 2
#' @param liver_s9 (mg S9/g liver) S9 content of liver tissue
#' @param liver_to_body_frac (unitless) Liver body weight fraction with default of 0.015
#' @param in_vitro_clearance (ml assay/ mg S9/h) Intrinsic in vitro clearance rate
#' @return (ml assay/(g liver * day)) intrinsic in vivo clearance
#' @export
vitro_to_vivo_clearance <- function(reaction_rate = NA, assay_s9 = 2, liver_s9 = 163,
                                    liver_to_body_frac = 0.015, in_vitro_clearance = NA){

  if (is.na(reaction_rate) && is.na(in_vitro_clearance)){
    stop("Either have to give the reaction rate or in vitro clearance rate")
  }
  else if(is.na(in_vitro_clearance)){
    in_vitro_clearance <- reaction_rate/assay_s9
  }

  in_vivo_clearance <- 24*in_vitro_clearance*liver_s9*liver_to_body_frac

  return(in_vivo_clearance)
}

#' Calculate the liver blood flow
#'
#' @param biota_mass (kg) Total wet weight of the fish
#' @param water_temp (Celsius) The water temperature
#' @param cardiac_output  Cardiac output fraction to liver. Default value is 0.259
#' @return (L/kg/days) The  Blood flow to liver
calc_liver_blood_flow <- function(biota_mass, water_temp = 25, cardiac_output = 0.259){

  if(biota_mass<=0){
    stop("The biota mass has to be greater than 0")
  }
  mass_trans <- (biota_mass*1000/500)^(-0.1)

  water_trans <- (0.23* water_temp)-0.78

  liver_blood_flow <- cardiac_output*24*water_trans*mass_trans

  return(liver_blood_flow)
}

#' Calculate the biotranformation clearance rate
#'
#' Derived by Nichols
#' @param liver_blood_flow (L/kg/days) The  Blood flow to liver. See \link[BIONICv3]{calc_liver_blood_flow}
#' @param in_vivo_clearance (ml assay/(g liver * day))intrinsic in vivo clearance. See \link[BIONICv3]{vitro_to_vivo_clearance}
#' @param dfw_assay  (unitless)Assay-water distribution ratio
#' @param dfw_blood_lysosome (L/L) Blood-water distribution ratio
#' @param dfw_biotransformation_lysosome (L/kg) Fish-water distribution ratio (IVIVE), adjusted for lysosome sequestration
#' @return (1/days) Biotransformation rate constant
calc_biotransformation_rate <- function(liver_blood_flow, in_vivo_clearance, dfw_assay,
                                        dfw_blood_lysosome, dfw_biotransformation_lysosome){

  if(dfw_blood_lysosome==0 || dfw_biotransformation_lysosome==0){
    stop("Division by zero error")
  }
  in_vitro_trans <- in_vivo_clearance*(dfw_assay/dfw_blood_lysosome)

  if(liver_blood_flow+in_vitro_trans==0){
    stop("Division by zero error")
  }

  hepatic_clearance <- (liver_blood_flow*in_vitro_trans)/(liver_blood_flow+in_vitro_trans)

  biotransformation_rate <- hepatic_clearance/(dfw_biotransformation_lysosome/dfw_blood_lysosome)

  return(biotransformation_rate)
}
