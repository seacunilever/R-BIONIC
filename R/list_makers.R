# These function just turn individual variables to a list, its an inconveniences to this code
#but I haven't found a different solution for this algo - Prehaps solution have a function that has ... for parameters

list_vol_fracs <- function(vol_frac_sl, vol_frac_pl, vol_frac_sa, vol_frac_pw,
                           vol_frac_water=NA){
  if (is.na(vol_frac_water)){
    vol_frac_water <- 1 - (vol_frac_sl+ vol_frac_pl+vol_frac_sa+vol_frac_pw)
  }
  vol_frac_list <- list("sl"=vol_frac_sl, "pl"=vol_frac_pl, "sa"=vol_frac_sa, "pw"=vol_frac_pw,
                        "water"=vol_frac_water)
  return(vol_frac_list)
}

list_assay_vol_fracs <- function(assay_vol_frac_sl, assay_vol_frac_pl, assay_vol_frac_sa,
                                 assay_vol_frac_pw, assay_vol_frac_water=NA){
  if (is.na(assay_vol_frac_water)){
    assay_vol_frac_water <- 1 - (assay_vol_frac_sl+assay_vol_frac_pl+assay_vol_frac_sa+assay_vol_frac_pw)
  }
  assay_vol_frac_list <- list("sl"=assay_vol_frac_sl, "pl"=assay_vol_frac_pl,
                              "sa"=assay_vol_frac_sa, "pw"=assay_vol_frac_pw,
                              "water"=assay_vol_frac_water)
  return(assay_vol_frac_list)
}

list_blood_vol_fracs <- function(blood_vol_frac_sl, blood_vol_frac_pl, blood_vol_frac_sa,
                                 blood_vol_frac_pw, blood_vol_frac_water=NA){
  if (is.na(blood_vol_frac_water)){
    blood_vol_frac_water <- 1 - (blood_vol_frac_sl+blood_vol_frac_pl+blood_vol_frac_sa+blood_vol_frac_pw)
  }
  blood_vol_frac_list <- list("sl"=blood_vol_frac_sl, "pl"=blood_vol_frac_pl,
                              "sa"=blood_vol_frac_sa, "pw"=blood_vol_frac_pw,
                              "water"=blood_vol_frac_water)
  return(blood_vol_frac_list)
}

list_density <- function(density_sl, density_pl, density_sa, density_pw){

  density_list <- list("sl"=density_sl, "pl"=density_pl,
                       "sa"=density_sa, "pw"=density_pw)
  return(density_list)
}

list_partition <- function(KOWN, KSWN, KSWI, KMWN, KMWI, KSaWN, KSaWI, KPWN, KPWI){

  partition_list <- list("KOWN"=KOWN, "KSWN"=KSWN, "KSWI"=KSWI, "KMWN"=KMWN,
                         "KMWI"=KMWI, "KSaWN"=KSaWN, "KSaWI"=KSaWI, "KPWN"=KPWN,
                         "KPWI"=KPWI)

  return(partition_list)
}

list_distribution_ratio <- function(DSW, DMW, DMWAN, DSAW, DPW){

  distribution_ratio_list <- list("DSW"=DSW, "DMW"=DMW, "DMWAN"=DMWAN,
                                  "DSAW"=DSAW, "DPW"=DPW)

  return(distribution_ratio_list)
}

list_biotransformation_distribution_ratio <- function(DSW_biotransformation, DMW_biotransformation,
                                                      DMWAN_biotransformation, DSAW_biotransformation,
                                                      DPW_biotransformation){

  biotransformation_dist_ratio_list <- list("DSW"=DSW_biotransformation, "DMW"=DMW_biotransformation, "DMWAN"=DMWAN_biotransformation,
                                        "DSAW"=DSAW_biotransformation, "DPW"=DPW_biotransformation)

  return(biotransformation_dist_ratio_list)
}
list_diet_vol_frac <- function(diet_vol_frac_sl, diet_vol_frac_pl, diet_vol_frac_sa, diet_vol_frac_pw){
  diet_vol_frac_list <- list(sl = diet_vol_frac_sl, pl = diet_vol_frac_pl, sa = diet_vol_frac_sa, pw = diet_vol_frac_pw)
  return(diet_vol_frac_list)
}

list_assimilation_eff <- function(assimilation_eff_l, assimilation_eff_nlom, assimilation_eff_w){
  assimilation_eff_list <- list(l=assimilation_eff_l, nlom=assimilation_eff_nlom, w=assimilation_eff_w)
  return(assimilation_eff_list)
}
