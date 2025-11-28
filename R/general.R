#' Calculate ionic form of partition
#'
#' This equation calculates the ionic form of partition using the neutral partition
#' and a scaling factor. The Scaling factor depends on the partition.
#' @param partition_neut (unitless) This is the neutral partition value for example the membrane water partition
#' @param scaling_factor (unitless) This is the scaling factor depended on the partitions, the default values for these
#' are found in Armitage 2012 Supplementary Table S7.
#' @return (unitless) The ionic form of the partition
calc_partition_ion <- function(partition_neut, scaling_factor){

  log_partition_ion <- log10(partition_neut) - scaling_factor

  partition_ion <- 10^(log_partition_ion)

  return(partition_ion)
}

#' Calculate neutral form of partition
#'
#' This equation calculates the partition of neutral form using the octanol water
#' partition coefficient and coefficients based on the regressions values in literature
#' @param oct_water_neut (unitless) The octanol water partition coefficient of a chemical
#' @param coeff_a (unitless) This is a regression based value that acts as the gradient for the equation.
#' For example, for the membrane-water coefficient the value is 1.01 in Endo 2011
#' @param coeff_b (unitless) This is a regression based value that acts as the y-intercept for the equation.
#' For example, for the membrane-water coefficient the value is 0.12 in Endo 2011
#' @return (unitless) the neutral form of the partition coefficient
calc_partition_neut <- function(oct_water_neut, coeff_a, coeff_b){

  if(oct_water_neut<=0){
    stop("The KOWN is less then or equal to 0")
  }

  log_partition_neut <- coeff_a*log10(oct_water_neut) + coeff_b

  partition_neut <- 10^(log_partition_neut)

  return(partition_neut)
}

#' Calculate neutral form of serum-albumin-water partition
#'
#' This equation differs from \link[BIONICv3]{calc_partition_neut} due to the regression
#' parameters changing based off of the octanol water coefficient
#' @inheritParams calc_partition_neut
#' @return (unitless) the neutral form of the serum-albumin-water partition coefficient
calc_partition_sawn <- function(oct_water_neut){

  if (log10(oct_water_neut)<4.5){
    partition_sawn <- calc_partition_neut(oct_water_neut, 1.08, -0.7)
  }
  else{
    partition_sawn <- calc_partition_neut(oct_water_neut, 0.37, 2.56)
  }

  return(partition_sawn)
}

#' Calculate the neutral form of octanol-water partition
#'
#' This equation is the reverse of \link[BIONICv3]{calc_partition_neut} in case the
#' value of octanol-water partition is not known but other partitions are e.g. the
#' membrane-water partition coefficient.
#' @param partition_neut (unitless) The known neutral partition coefficient
#' @inheritParams calc_partition_neut
#' @return The octonal-water partition in it's neutral form
calc_oct_water_neut <- function(partition_neut, coeff_a, coeff_b){

  if(partition_neut<=0){
    stop("The partition neut is less then or equal to 0")
  }
  if(coeff_a==0){
    stop("coeff_a cannot be 0 due to division by 0")
  }


  log_oct_water_neut <- (log10(partition_neut) - coeff_b)/coeff_a

  oct_water_neut <- 10^(log_oct_water_neut)

  return(oct_water_neut)
}

#' Calculate RATIO parameter
#'
#' The ratio equation are based on the Henderson-Hasselbach equations and they depends on
#' the ion type value
#' @param ph (unitless) The pH (potential of hydrogen) of a particular environment
#' @param pka (unitless) The pKa of the chemical
#' @param ion_type (unitless) This is ionic type of the chemical which includes "A"(Acid), "B"(BAse), "N"(Neutral), "Q"(QAC)
#' @return (unitless) The ratio parameter
calc_ratio <- function(ph, pka, ion_type){

  exponent <- ph - pka

  #basic reverses the equation
  exponent_ion <- exponent* ifelse(ion_type=="B", -1, 1)

  ratio <- 10^(exponent_ion)

  return(ratio)
}

#' Calculate the speciation at neutral form
#'
#' This calculates the speciation at a certain environments pH for a certain chemical of the neutral form.
#' @param ratio (unitless) The ratio parameter. See: \link[BIONICv3]{calc_ratio}
#' @return (unitless) speciation at neutral form
calc_speciation_neut <- function(ratio){

  if(ratio<0){
    stop("The ratio calculation cannot be less than 0")
  }

  speciation_neut <- 1/(1+ratio)


  return(speciation_neut)
}

#' Calculate the speciation at ion form
#'
#' This calculates the speciation at a certain environments pH for a certain chemical of the ion form.
#' To do this it is just 1- the speciation
#' @param spectiation_neut speciation at neutral form
#' @return (unitless) speciation at ion form
calc_speciation_ion <- function(spectiation_neut){

  if(spectiation_neut<0 || spectiation_neut>1){
    stop("The speciation_neut has to be between 0 and 1 (0 and 1 included)")
  }

  speciation_ion <- 1- spectiation_neut

  return(speciation_ion)
}

#' Calculate ther distribution ratio
#'
#' The distribution ratio depends on both the neutral form and ionic form of the partition coefficient.
#' The equation is derived from Schmitt 2008
#' @param partition_neut (unitless) This is the neutral partition value
#' @param partition_ion (unitless) The ionic form of the partition
#' @param speciation_neut (unitless) speciation at neutral form
#' @param speciation_ion (unitless) speciation at ion form
#' @return (unitless) The distribution ratio specific to that partition
calc_distribution_ratio <- function(partition_neut, partition_ion, speciation_neut,
                                    speciation_ion){
  distribution_ratio <- speciation_neut*partition_neut + speciation_ion*partition_ion

  return(distribution_ratio)
}

#' calculate the membrane-water distribution ratio for anionic phospholipds
#'
#' Calculates the membrane-water distribution ratio to handle sopriton to anionic
#' phospholipids
#' @param KMWN (unitless) Membrane water partition coefficient in neutral form
#' @param KMWI (unitless) Membrane water partition coefficient in ionic form
#' @param speciation_neut (unitless) speciation at neutral form
#' @param speciation_ion (unitless) speciation at ion form
#' @param asorp_chem (unitless) - Proportionality constant to increase/decrease partitioning to the
#'fraction of the total pool of phospholipids. See \link[BIONICv3]{calc_asorp_chem}
#' @return (unitless) the membrane-water distribution ratio to handle sopriton to anionic
#' phospholipids
calc_distribution_ratio_mwan <- function(KMWN, KMWI, speciation_neut, speciation_ion,
                                         asorp_chem){

  distribution_ratio <- speciation_neut*KMWN + speciation_ion*KMWI* asorp_chem

}

#' Calculate the volume fraction
#'
#' Calculates the volume fraction using mass/density. Probably wrong as the units are wrong
#' @param mass (kg) The mass of a partition
#' @param density (kg/(m^3)) The density of a partition
#' @return (unitless) The volume fraction
calc_bionic_volume_fraction <- function(mass, density){
  volume_fraction <- mass/density
  return(volume_fraction)
}

#' Calculate the assay volume fraction
#'
#' Similar to \link[BIONICv3]{calc_bionic_volume_fraction} with the addition to assay S9
#' @inheritParams calc_bionic_volume_fraction
#' @param assay_s9 The assay S9 value used
#' @return (unitless) The assay volume fraction
calc_assay_volume_fraction <- function(mass, density, assay_s9){
  volume_fraction <- mass/density

  volume_fraction_adj <- volume_fraction*assay_s9/2

  return(volume_fraction_adj)
}

#' Calculate the fish-water distribution ratio
#'
#' Add more later here
#' @param vol_frac_list A volume fraction list including values for sl, sa, pw, pl and water
#' @param distribution_ratio_list A list of the distribution ratios including DSW, DSAW,
#' DPW, DMW, DMWAN
#' @param anionic_frac - Fraction of membrane lipid that is anionic phospholipid. Default is 0.175. From Schmitt 2008
#' @return distribution ratio between fish and water
calc_dfw <- function(vol_frac_list, distribution_ratio_list, anionic_frac = 0.175){

  vfl <- vol_frac_list
  drl <- distribution_ratio_list

  dfw_non_anionic <- vfl$sl*drl$DSW + vfl$sa*drl$DSAW + vfl$pw*drl$DPW + vfl$water
  dfw_anionic <- vfl$pl*((1-anionic_frac)*drl$DMW + anionic_frac*drl$DMWAN)

  dfw <- dfw_non_anionic+dfw_anionic

  return(dfw)

}

#' calculate pH at gill
#'
#' This equation is used when the pH is not available using the pH bulk based on Erickson
#' @param ph_bulk (unitless) The pH at bulk water which is defaulted as 7
#' @return (unitless) The pH at the biotas gill
calc_ph_gill <- function(ph_bulk = 7){

  ph_gill <- 0.7141*ph_bulk + 1.4672

  return(ph_gill)
}

#'Calculate the asorption chemical value
#'
#'A filler function to get the right asorption chemical based on the ion type
calc_asorp_chem <- function(ion_type, asorp_neut, asorp_acid, asorp_base, asorp_qac){
  asorp_list <- list("N"=asorp_neut, "A"=asorp_acid, "B"=asorp_base, "Q"=asorp_qac)

  asorp_chem <- asorp_list[[ion_type]]
  return(asorp_chem)
}

#holder function
calc_delta_from_ow <- function(delta_ow){
  return(delta_ow)
}

#'Get Scaling factor
#'
#'depending on the ion type the scaling factor changes so this function is placed to get the right
#'defualt value
#'
#' @param ion_type The ion type of the chemical
#' @param delta_acid If the ion_type is acid then this value is used
#' @param delta_base If the ion type is base then this value is used
calc_delta_from_ion <- function(ion_type, delta_acid, delta_base){
  if (ion_type=="A"){
    return(delta_acid)
  }
  else if(ion_type=="B"){
    return(delta_base)
  }
  else{
    return(0)
  }
}
