#' Calculate Lyso Pump
#'
#' Calculates the movement of the chemical in the lysosome which is dependent on
#' ionisation of the chemical
#' @param ion_type (unitless)The ionisation of the chemical which includes "A"(Acid), "B"(Base),
#' "N"(Neutral) and "Q"(QAC)
#' @param pka (unitless)The acid-dissociation constant
#' @param ph_lyso (unitless)pH of Lysosome which is defaulted as 5.1
#' @param ph_bio (unitless)The biological pH which is defaulted as 7.8
#' @return (unitless)Ionisation dependent movement of chemical into lysosome
calc_lyso_pump <- function(ion_type, pka, ph_lyso = 5.1, ph_bio = 7.8){

  if (ion_type=="B"){
    ratio_lyso <- 1+10^(pka - ph_lyso)
    ratio_bio <- 1+10^(pka - ph_bio)
    lyso_pump <- ratio_lyso/ ratio_bio
  }
  else{
    lyso_pump <- 1
  }

  return(lyso_pump)
}

#' Calculate intralysosomal membrane volume fraction
#'
#' This equation calculates the intralysosomal membrane volume faction due to being assumed that
#' only the inner layer of the membrane is relevant for absorption. therefore the volumes are calculated
#' and ratioed of how much the volume the membrane holds
#' @param lyso_diam (nano-meters) This is the lysosome average diameter which is defaulted as 520nm
#' @param lyso_thick (nano-meters) This is the lysosome membrane thickness defaulted as 4nm
#' @return (unitless) The intralysosomal membrane volume fraction
calc_lyso_membrane_vol_frac <- function(lyso_diam = 520, lyso_thick = 4){

  if (lyso_diam<=0){
    stop("The lysosome diameter has to be greater than 0")
  }

  if (lyso_thick>=(lyso_diam/2)){
    stop("The thickness of the membrane has to be lower than the lysosome radius")
  }

  #volume of the lysosome
  lyso_tv <- (4/3)*pi*((lyso_diam/2)^3)

  #volume of the inside of the lysosome without membrane
  lyso_tv_thick <- (4/3)*pi*((lyso_diam/2) - lyso_thick)^3

  #gets the volume of the membrane
  lyso_mv <- lyso_tv-lyso_tv_thick

  #not sure why there is a division of 2, probably assay
  lyso_membrane_vol_frac <- (lyso_mv/lyso_tv)/2

  return(lyso_membrane_vol_frac)
}

#' Calculate the dissociation ratio between the lysosome and water
#'
#' @param distribution_ratio_list a list of distribution ratios including DMW, DMWan
#' @param lyso_membrane_vol_frac (unitless)  intralysosomal membrane volume see more: \link[BIONICv3]{calc_lyso_membrane_vol_frac}
#' @param lyso_pump (unitless)  ionisation dependent movement of chemical into lysosome see more: \link[BIONICv3]{calc_lyso_pump}
#' @param anionic_frac (unitless) Fraction of total phospholipids that are anionic. Default is 0.175
#' @return (unitless) The dissociation ratio between the lysosome and water
calc_dlysw <- function(distribution_ratio_list, lyso_membrane_vol_frac, lyso_pump,
                       anionic_frac = 0.175){

  lyso_wvf <- 1-lyso_membrane_vol_frac

  non_anionic_calc <- (1-anionic_frac)*lyso_membrane_vol_frac*distribution_ratio_list$DMW
  anionic_calc <- anionic_frac*lyso_membrane_vol_frac*distribution_ratio_list$DMWAN

  dlysw <-  (non_anionic_calc+anionic_calc+lyso_wvf)* lyso_pump

  return(dlysw)
}

#' Apply lysosomal sequestration on distribution ratio
#'
#' depending on the whole body fraction for lysosome is the amount of the lysosome-water
#' distribution ratio.
#' @param dfw the distribution ratio gotten from fish to water. See more: \link[BIONICv3]{calc_dfw}
#' @param dlysw (unitless) lysosome-water distribution ratio. See more: \link[BIONICv3]{calc_dlysw}
#' @param lyso_to_body_frac (unitless) The whole body fraction for lysosome. Default at 0.0016
#' @return The applied lysosomal sequestration on the distribution ratio
#' @export
apply_lyso_seq_dfw <- function(dfw, dlysw, lyso_to_body_frac = 0.0016){

  dfw_lys <- (1-lyso_to_body_frac)*dfw + lyso_to_body_frac*(dlysw)

  return(dfw_lys)
}
