#'Calculates the undigest_list
#'
#'Calculates the temporary variable used for further calculations in other functions
#' @param diet_vol_frac_list A list of diet volume fractions
#' @param density_list A list of the densities in different partitions
#' @param assimilation_eff_list A list of the assimilation efficiencies including lipids, NLOMs and water
calc_undigest_list <- function(diet_vol_frac_list = NA, density_list = NA, assimilation_eff_list = NA){

  vol_map_ass <- list(sl = "l", pl = "l", sa = "nlom", pw = "nlom", w = "w")

  if (any(density_list==0)){
    stop("none of the density list can be 0")
  }

  #gets the volume fractions of each partition
  fd <- list()
  for (i in names(diet_vol_frac_list)) {
    fd[[i]] <- diet_vol_frac_list[[i]]/density_list[[i]]
  }
  fd$w <- 1- sum(unname(unlist(fd)))

  #follows q = (1-a)*Fd
  undigest_list <-list()
  for (i in names(fd)){
    undigest_list[[i]] <- (1-assimilation_eff_list[[vol_map_ass[[i]]]])*fd[[i]]
  }
 return(undigest_list)
}

#' Calculates the fecal volume fraction list
#'
#' Calculates the fecal volume fraction list from the calculated undigest_list list which is
#' calculated by finding the fraction of the partition is part of from the whole sum of the undigest_list
#' @param undigest_list A list of temp variables that measures the amount of partition NOT assimilated(absorbed).
#' @return The fecal volume fraction list
calc_fecal_vol_frac_list <- function(undigest_list){

  if (sum(unname(unlist(undigest_list)))==0){
    stop("The sum of undigest_list cannot be 0")
  }

  #finds the fraction between the sum of undigest for each undigest
  fecal_vol_frac_list <- list()
  for (i in names(undigest_list)){
    fecal_vol_frac_list[[i]] <- undigest_list[[i]]/(sum(unname(unlist(undigest_list))))
  }

  return(fecal_vol_frac_list)
}

#' Calculate diet_rate
#'
#' @param biota_mass (kg)The mass of the biota used for this run
#' @param water_temp (Celsius) The temperature of the water the biota is in
#' @return Fill later
calc_diet_rate <- function(biota_mass, water_temp = 25){

  diet_rate <- 0.022*(biota_mass^0.85)*exp(0.06*water_temp)

  return(diet_rate)
}

#' Calculate the fecal rate
#'
#' @param undigest_list These are temp variables that measures the amount of partition NOT assimilated(absorbed)
#' @param diet_rate Fill later
#' @return Fill later
calc_fecal_rate <- function(undigest_list, diet_rate){

  fecal_rate <- sum(unname(unlist(undigest_list)))*diet_rate

  return(fecal_rate)
}

#' Calculate the dietary uptake
#'
#' The absorption efficiency equation was defined by Gobas [21], where the absorption efficiency
#' data and octonal water partition coefficient was gathered from literature, specifically fish that
#' were exposed only to contaminated food for a long period of time with a constant feeding rate.
#' After that using the non-linear regression(SAS NLIN) to fit for parameter
#'
#' @param partition_list A list of the partitions used in the run including KMWI and KMWN
#' @param ion_type The ionisation type of the chemical being ran
#' @param dietary_uptake_a (unitless)Coefficient gathered from the regression. Default is 0.000000053
#' @param dietary_uptake_b (unitless)Coefficient gathered from the regression. Default is 2
calc_dietary_uptake <- function(partition_list, ion_type, dietary_uptake_a = 0.000000053,
                                dietary_uptake_b = 2){


  if (ion_type=="Q"){
    dietary_uptake <- dietary_uptake_a*partition_list$KMWI+dietary_uptake_b
  }
  else{
    dietary_uptake <- dietary_uptake_a*partition_list$KOWN+dietary_uptake_b
  }

  if(dietary_uptake == 0){
    stop("the sum cannot be 0 before the inverse")
  }

  dietary_uptake <- (dietary_uptake)^(-1)

  return(dietary_uptake)
}

#' Calculate the git partition or KGB
#'
#' This value is the ratio of the partitioning into the gut divided by the total partitioning to the fish
#' @param fecal_vol_frac_list The volume fraction of the faecal material of partition. See \link[BIONICv3]{"calc_fecal_vol_frac_list"}
#' @param distribution_ratio_list The list of distribution ratios for the partitions
#' @param dfw_uptake_lysosome (unitless) The fish-water distribution ratio, recalculated to account for lysosomal sequestration.
#' This is the same one used for gill elimination rate
#' @return The ratio of the partitioning into the gut divided by the total partitioning to the fish
calc_git_partition <- function(fecal_vol_frac_list, distribution_ratio_list, dfw_uptake_lysosome){

  if(dfw_uptake_lysosome==0){
    stop("Division by zero error")
  }

  fvfl <- fecal_vol_frac_list
  drl <- distribution_ratio_list

  part_sum <- fvfl$sl*drl$DSW +  fvfl$pl*drl$DMW + fvfl$sa*drl$DSAW + fvfl$pw*drl$DPW + fvfl$w

  git_partition <- part_sum/dfw_uptake_lysosome

  return(git_partition)
}

#' Calculate the fecal egestion rate
#'
#' @param biota_mass (kg) The mass of the biota used in the run
#' @param git_partition The ratio of the partitioning into the gut divided by the total partitioning to the fish
#' @param dietary_uptake (unitless) The absorption efficiency of chemical from food
#' @param fecal_rate Finish this later
#' @return (1/day)The faecal egestion rate constant
calc_fecal_egestion_rate <- function(biota_mass, git_partition, dietary_uptake, fecal_rate){

  if(biota_mass<=0){
    stop("The biota mass cannot be less or equal to 0")
  }

  fecal_egestion_rate <- (git_partition/biota_mass)*dietary_uptake*fecal_rate

  return(fecal_egestion_rate)
}
