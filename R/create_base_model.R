#' Create BIONICV3 graph model
#'
#' Creates the BIONICv3 graph model using the model_graph_construction. In the future this can be
#' saved as an R object instead of creating the graph model every single time
#' @return A list including the root, edges dataframe and vertices list for BIONICv3.
create_bionicv3_model <- function(){

  #BCF - the root of the graph
  #BCF
  g_data <- create_base_graph("bcf", "calc_bcf")

  #GROWTH ELMINATION RATE
  g_data <- add_model_to_graph(g_data, "growth_rate", "f", "calc_growth_rate")

  #GILL UPTAKE
  g_data <- add_model_to_graph(g_data, "gill_uptake_rate", "f", "calc_gill_uptake_rate")
  g_data <- add_model_to_graph(g_data, "gill_vent_rate", "f", "calc_gill_vent_rate")
  g_data <- add_model_to_graph(g_data, "metabolic_oxygen", "f", "calc_metabolic_oxygen")
  g_data <- add_model_to_graph(g_data, "oxygen_conc_water", "f", "calc_oxygen_conc_water")
  g_data <- add_model_to_graph(g_data, "gill_uptake_efficiency", "f", "calc_gill_uptake_efficiency")

  #GILL ELIMINATION RATE
  g_data <- add_model_to_graph(g_data, "gill_elimination_rate", "f", "calc_gill_elimination_rate")

  #BIOTRANSFORMATION RATE
  g_data <- add_model_to_graph(g_data, "biotransformation_rate", "f", "calc_biotransformation_rate")
  g_data <- add_model_to_graph(g_data, "liver_blood_flow", "f", "calc_liver_blood_flow")
  g_data <- add_model_to_graph(g_data, "in_vivo_clearance", "f", "vitro_to_vivo_clearance")

  #FECAL EGESTION RATE
  g_data <- add_model_to_graph(g_data, "fecal_egestion_rate", "f", "calc_fecal_egestion_rate")
  g_data <- add_model_to_graph(g_data, "git_partition", "f", "calc_git_partition")
  g_data <- add_model_to_graph(g_data, "dietary_uptake", "f", "calc_dietary_uptake")
  g_data <- add_model_to_graph(g_data, "fecal_rate", "f", "calc_fecal_rate")
  g_data <- add_model_to_graph(g_data, "diet_rate", "f", "calc_diet_rate")
  g_data <- add_model_to_graph(g_data, "undigest_list", "f", "calc_undigest_list")
  g_data <- add_model_to_graph(g_data, "fecal_vol_frac_list", "f", "calc_fecal_vol_frac_list")

  # PH SPECIFIC STUFF
  g_data <- add_model_to_graph(g_data, "ratio_gill", "f", "calc_ratio", c("ph_gill", "pka", "ion_type"))
  g_data <- add_model_to_graph(g_data, "ratio_bio", "f", "calc_ratio", c("ph_bio", "pka", "ion_type"))
  g_data <- add_model_to_graph(g_data, "ratio_bulk", "f", "calc_ratio", c("ph_bulk", "pka", "ion_type"))
  g_data <- add_model_to_graph(g_data, "ph_gill", "f", "calc_ph_gill")
  g_data <- add_model_to_graph(g_data, "speciation_gill_neut", "f", "calc_speciation_neut", c("ratio_gill"))
  g_data <- add_model_to_graph(g_data, "speciation_gill_ion", "f", "calc_speciation_ion", c("speciation_gill_neut"))

  #APPLYING lYSOSOME SEQUESTRATION
  g_data <- add_model_to_graph(g_data, "dfw_uptake_lysosome", "f", "apply_lyso_seq_dfw", c("dfw_elimination", "dlysw_elimination", "lyso_to_body_frac"))
  g_data <- add_model_to_graph(g_data, "dfw_biotransformation_lysosome", "f", "apply_lyso_seq_dfw", c("dfw_biotransformation", "dlysw_biotransformation", "lyso_to_body_frac"))
  g_data <- add_model_to_graph(g_data, "dfw_blood_lysosome", "f", "apply_lyso_seq_dfw", c("dfw_blood", "dlysw_biotransformation", "lyso_to_blood_frac"))

  #CALCLULATING X-ASSAU DISTRIBUTION RATIOS
  g_data <- add_model_to_graph(g_data, "dfw_assay", "f", "calc_dfw", c("assay_vol_frac_list", "biotransformation_dist_ratio_list", "anionic_frac"))
  g_data <- add_model_to_graph(g_data, "dfw_blood", "f", "calc_dfw", c("blood_vol_frac_list", "biotransformation_dist_ratio_list", "anionic_frac"))
  g_data <- add_model_to_graph(g_data, "dfw_biotransformation", "f", "calc_dfw", c("vol_frac_list", "biotransformation_dist_ratio_list", "anionic_frac"))
  g_data <- add_model_to_graph(g_data, "dfw_elimination", "f", "calc_dfw", c("vol_frac_list", "distribution_ratio_list", "anionic_frac"))

  g_data <- add_model_to_graph(g_data, "dlysw_biotransformation", "f", "calc_dlysw", c("biotransformation_dist_ratio_list", "lyso_membrane_vol_frac",
                                                                           "lyso_pump", "anionic_frac"))
  g_data <- add_model_to_graph(g_data, "dlysw_elimination", "f", "calc_dlysw", c("distribution_ratio_list", "lyso_membrane_vol_frac",
                                                                        "lyso_pump", "anionic_frac"))

  g_data <- add_model_to_graph(g_data, "lyso_membrane_vol_frac", "f", "calc_lyso_membrane_vol_frac")
  g_data <- add_model_to_graph(g_data, "lyso_pump", "f", "calc_lyso_pump")

  #List makers
  g_data <- add_model_to_graph(g_data, "vol_frac_list", "f", "list_vol_fracs")
  g_data <- add_model_to_graph(g_data, "assay_vol_frac_list", "f", "list_assay_vol_fracs")
  g_data <- add_model_to_graph(g_data, "blood_vol_frac_list", "f", "list_blood_vol_fracs")
  g_data <- add_model_to_graph(g_data, "density_list", "f", "list_density")
  g_data <- add_model_to_graph(g_data, "partition_list", "f", "list_partition")
  g_data <- add_model_to_graph(g_data, "distribution_ratio_list", "f", "list_distribution_ratio")
  g_data <- add_model_to_graph(g_data, "biotransformation_dist_ratio_list", "f", "list_biotransformation_distribution_ratio")
  g_data <- add_model_to_graph(g_data, "diet_vol_frac_list", "f", "list_diet_vol_frac")
  g_data <- add_model_to_graph(g_data, "assimilation_eff_list", "f", "list_assimilation_eff")

  #Distribution ratios
  g_data <- add_model_to_graph(g_data, "DSW", "f", "calc_distribution_ratio", c("KSWN", "KSWI", "speciation_bio_neut", "speciation_bio_ion"))
  g_data <- add_model_to_graph(g_data, "DMW", "f", "calc_distribution_ratio", c("KMWN", "KMWI", "speciation_bio_neut", "speciation_bio_ion"))
  g_data <- add_model_to_graph(g_data, "DSAW", "f", "calc_distribution_ratio", c("KSaWN", "KSaWI", "speciation_bio_neut", "speciation_bio_ion"))
  g_data <- add_model_to_graph(g_data, "DPW", "f", "calc_distribution_ratio", c("KPWN", "KPWI", "speciation_bio_neut", "speciation_bio_ion"))
  g_data <- add_model_to_graph(g_data, "DMWAN", "f", "calc_distribution_ratio_mwan", c("KMWN", "KMWI", "speciation_bio_neut", "speciation_bio_ion", "asorp_chem"))

  g_data <- add_model_to_graph(g_data, "DSW_biotransformation", "f", "calc_distribution_ratio", c("KSWN", "KSWI", "speciation_biotransformation_neut", "speciation_biotransformation_ion"))
  g_data <- add_model_to_graph(g_data, "DMW_biotransformation", "f", "calc_distribution_ratio", c("KMWN", "KMWI", "speciation_biotransformation_neut", "speciation_biotransformation_ion"))
  g_data <- add_model_to_graph(g_data, "DSAW_biotransformation", "f", "calc_distribution_ratio", c("KSaWN", "KSaWI", "speciation_biotransformation_neut", "speciation_biotransformation_ion"))
  g_data <- add_model_to_graph(g_data, "DPW_biotransformation", "f", "calc_distribution_ratio", c("KPWN", "KPWI", "speciation_biotransformation_neut", "speciation_biotransformation_ion"))
  g_data <- add_model_to_graph(g_data, "DMWAN_biotransformation", "f", "calc_distribution_ratio_mwan", c("KMWN", "KMWI", "speciation_biotransformation_neut", "speciation_biotransformation_ion", "asorp_chem"))

  g_data <- add_model_to_graph(g_data, "speciation_bio_neut", "f", "calc_speciation_neut", c("ratio_bio"))
  g_data <- add_model_to_graph(g_data, "speciation_bio_ion", "f", "calc_speciation_ion", c("speciation_bio_neut"))
  g_data <- add_model_to_graph(g_data, "speciation_biotransformation_neut", "f", "calc_speciation_neut", c("ratio_biotransformation"))
  g_data <- add_model_to_graph(g_data, "speciation_biotransformation_ion", "f", "calc_speciation_ion", c("speciation_biotransformation_neut"))

  g_data <- add_model_to_graph(g_data, "ratio_biotransformation", "f", "calc_ratio", c("ph_biotransformation", "pka", "ion_type"))
  g_data <- add_model_to_graph(g_data, "asorp_chem", "f", "calc_asorp_chem")

  #partitions
  g_data <- add_model_to_graph(g_data, "KSWN", "f", "calc_partition_neut", c("KOWN", "KSWN_coeff_a", "KSWN_coeff_b"))
  g_data <- add_model_to_graph(g_data, "KSWI", "f", "calc_partition_ion", c("KSWN", "delta_sw"))
  g_data <- add_model_to_graph(g_data, "KMWN", "f", "calc_partition_neut", c("KOWN", "KMWN_coeff_a", "KMWN_coeff_b"))
  g_data <- add_model_to_graph(g_data, "KMWI", "f", "calc_partition_ion", c("KMWN", "delta_mw"))
  g_data <- add_model_to_graph(g_data, "KSaWN", "f", "calc_partition_sawn", c("KOWN"))
  g_data <- add_model_to_graph(g_data, "KSaWI", "f", "calc_partition_ion", c("KSaWN", "delta_saw"))
  g_data <- add_model_to_graph(g_data, "KPWN", "f", "calc_partition_neut", c("KOWN", "KPWN_coeff_a", "KPWN_coeff_b"))
  g_data <- add_model_to_graph(g_data, "KPWI", "f", "calc_partition_ion", c("KPWN", "delta_pw"))
  g_data <- add_model_to_graph(g_data, "KOWN", "f", "calc_oct_water_neut", c("KMWN", "KMWN_coeff_a", "KMWN_coeff_b"))


  #delta valuse
  g_data <- add_model_to_graph(g_data, "delta_sw", "f", "calc_delta_from_ow")
  g_data <- add_model_to_graph(g_data, "delta_mw", "f", "calc_delta_from_ion", c("ion_type", "delta_mw_acid", "delta_mw_base"))
  g_data <- add_model_to_graph(g_data, "delta_saw", "f", "calc_delta_from_ion", c("ion_type", "delta_saw_acid", "delta_saw_base"))
  g_data <- add_model_to_graph(g_data, "delta_pw", "f", "calc_delta_from_ow")

  parts <- c("sl", "pl", "sa", "pw")

  #fill the lists
  for (i in parts){
    g_data <- add_model_to_graph(g_data, paste0("vol_frac_",i), "f", "calc_bionic_volume_fraction",
                                 c(paste0("mass_frac_",i), paste0("density_", i)))

    g_data <- add_model_to_graph(g_data, paste0("blood_vol_frac_",i), "f", "calc_bionic_volume_fraction",
                                 c(paste0("blood_mass_frac_",i), paste0("density_", i)))

    g_data <- add_model_to_graph(g_data, paste0("assay_vol_frac_",i), "f", "calc_assay_volume_fraction",
                                 c(paste0("assay_mass_frac_",i), paste0("density_", i), "assay_s9"))
  }


  defaults <- c("water_temp", "eff_oxy_transfer", "saturation", "resistance_at_gill", "resistance_within_gill", "z_water_value",
                "gamma_eff", "beta_eff", "cardiac_output", "reaction_rate", "assay_s9",
                "liver_s9", "liver_to_body_frac", "in_vitro_clearance", "dietary_uptake_a",
                "dietary_uptake_b", "ph_bulk", "ph_bio", "anionic_frac", "lyso_to_body_frac",
                "lyso_to_blood_frac", "ph_lyso", "lyso_diam", "lyso_thick")
  defaults_vals <- c(25, 0.65, 0.9, 1.85, 155, 1, NA, 0.0001, 0.259, NA, 2, 163, 0.015,
                     NA,  0.000000053, 2, 7, 7.8, 0.175, 0.001568, 0.0005, 5.1,
                     520, 4)

  g_data <- add_defaults_to_graph(g_data, defaults, defaults_vals)

  defaults <- c("vol_frac_water", "density_sl", "density_pl", "density_sa", "density_pw","KSWN_coeff_a", "KSWN_coeff_b",
                "KMWN_coeff_a", "KMWN_coeff_b", "KPWN_coeff_a","KPWN_coeff_b", "blood_vol_frac_water",
                "assay_vol_frac_water")
  defaults_vals <- c(NA, 0.9, 1.015, 1.36, 1, 1, 0,
                     1.01, 0.12, 0.8, -0.68, NA, NA)

  g_data <- add_defaults_to_graph(g_data, defaults, defaults_vals)

  defaults <- c("ph_biotransformation", "asorp_neut", "asorp_acid", "asorp_base", "asorp_qac", "mass_frac_sl",
                "mass_frac_pl", "mass_frac_sa", "mass_frac_pw", "blood_mass_frac_sl", "blood_mass_frac_pl",
                "blood_mass_frac_sa", "blood_mass_frac_pw", "assay_mass_frac_sl", "assay_mass_frac_pl",
                "assay_mass_frac_sa", "assay_mass_frac_pw", "delta_ow", "delta_mw_acid", "delta_mw_base",
                "delta_saw_acid", "delta_saw_base")
  defaults_vals <- c(7.8, 1, 0.05, 20, 20, 0.0375, 0.0125, 0.003, 0.15, 0.007, 0.007, 0.0225, 0.1235,
                     0, 0.0005, 0, 0.002, 3, 2, 1, 0, 1)

  g_data <- add_defaults_to_graph(g_data, defaults, defaults_vals)

  defaults <- c("diet_vol_frac_sl", "diet_vol_frac_pl", "diet_vol_frac_sa", "diet_vol_frac_pw",
                "assimilation_eff_l", "assimilation_eff_nlom", "assimilation_eff_w")
  defaults_vals <- c(0.012, 0.003, 0, 0.15,
                     0.92, 0.6, 0.7)

  g_data <- add_defaults_to_graph(g_data, defaults, defaults_vals)

  g_data <- add_inputs_to_graph(g_data)

  #adds constraints of it being between 0 and 1
  bounded_by_one <- c("speciation_gill_neut", "speciation_gill_ion", "speciation_bio_neut", "speciation_bio_ion",
                      "speciation_biotransformation_neut", "speciation_biotransformation_ion",
                      "vol_frac_sl", "blood_vol_frac_sl", "assay_vol_frac_sl",
                      "vol_frac_pl", "blood_vol_frac_pl", "assay_vol_frac_pl",
                      "vol_frac_sa", "blood_vol_frac_sa", "assay_vol_frac_sa",
                      "vol_frac_pw", "blood_vol_frac_pw", "assay_vol_frac_pw",
                      "saturation", "liver_to_body_frac", "anionic_frac",
                      "lyso_to_body_frac", "lyso_to_blood_frac", "vol_frac_water",
                      "blood_vol_frac_water", "assay_vol_frac_water", "mass_frac_sl",
                      "mass_frac_pl", "mass_frac_sa", "mass_frac_pw", "blood_mass_frac_sl",
                      "blood_mass_frac_sa", "blood_mass_frac_pw","blood_mass_frac_pl",
                      "assay_mass_frac_sl", "assay_mass_frac_sa", "assay_mass_frac_pw",
                      "assay_mass_frac_pl")
  for (i in bounded_by_one){
    g_data <- add_constraint_to_graph(g_data, i, "range", "[0,1]")
  }

  #adds the ones that have to be positve and greater than 0
  positves <- c("lyso_diam", "lyso_thick", "density_sl", "density_pl", "density_sa",
                "density_pw", "biota_mass", "KOWN", "KMWN", "KSWN", "KPWN", "KSaWN")

  for (i in positves){
    g_data <- add_constraint_to_graph(g_data, i, "range", "(0,]")
  }

  ratios <- c("ratio_gill", "ratio_bio", "ratio_bulk", "ratio_biotransformation")

  for (i in ratios){
    g_data <- add_constraint_to_graph(g_data, i, "range", "[0,]")
  }

  #adds constraints on ones that cannot equal to 0
  non_zeros <- c("dfw_blood_lysosome", "dfw_biotransformation_lysosome",
                 "dfw_uptake_lysosome", "gamma_eff", "resistance_within_gill",
                 "eff_oxy_transfer", "oxygen_conc_water")

  for (i in non_zeros){
    g_data <- add_constraint_to_graph(g_data, i, "range", "[,]neq 0")
  }

  g_data <- add_constraint_to_graph(g_data, "ion_type", "choice", c("A", "B", "N", "Q"))
  return(g_data)
}

#' Create BIONICv3 graph model for QAC's
#'
#' Creates the BIONICv3 graph model using the model_graph_construction for QAC's. In the future this can be
#' saved as an R object instead of creating the graph model every single time
#' @return A list including the root, edges dataframe and vertices list for BIONICv3.
create_bionicv3_quart_model <- function(){

  #gets bionic v3 model to open up and add different models
  g_data <- create_bionicv3_model()
  g_data <- delete_inputs_from_graph(g_data)

  #gets the speciation, partition and ratio veties and deletes them
  specs <- names(g_data$vertices)[startsWith(names(g_data$vertices), "spec")]
  part_neut <- names(g_data$vertices)[grepl("K(\\w)+WN$", names(g_data$vertices))]
  part_ion <- names(g_data$vertices)[grepl("K(\\w)+WI$", names(g_data$vertices))]
  ratios <- c("ratio_gill", "ratio_bio", "ratio_bulk", "lyso_pump")
  delete_nodes <- c(specs, part_neut, part_ion, ratios)
  for (del in delete_nodes){
    g_data <- delete_model_from_graph(g_data,vertex_name = del)
  }

  #add models for ionic partition coefficient
  g_data <- add_model_to_graph(g_data, "KSWI", "f", "calc_partition_neut", c("KOWI", "KSWI_coeff_a", "KSWI_coeff_b"))
  g_data <- add_model_to_graph(g_data, "KSaWI", "f", "calc_partition_neut", c("KOWI", "KSaWI_coeff_a", "KSaWI_coeff_b"))
  g_data <- add_model_to_graph(g_data, "KPWI", "f", "calc_partition_neut", c("KOWI", "KPWI_coeff_a", "KPWI_coeff_b"))
  g_data <- add_model_to_graph(g_data, "KOWI", "f", "calc_partition_neut", c("KMWI", "KOWI_coeff_a", "KOWI_coeff_b"))

  #Add defaults
  defaults <- c("KOWI_coeff_a", "KOWI_coeff_b","KSWI_coeff_a", "KSWI_coeff_b","KSaWI_coeff_a",
                "KSaWI_coeff_b","KPWI_coeff_a", "KPWI_coeff_b", "KOWN", "KSWN", "KMWN", "KSaWN", "KPWN")
  default_vals <- c(1, -3, 1, 0, 0.71, 0.42, 0.8, -0.68,0,0,0,0,0)

  g_data <- add_defaults_to_graph(g_data, defaults, default_vals)

  defaults <- c("speciation_gill_neut", "speciation_bio_neut", "speciation_biotransformation_neut",
                "speciation_gill_ion", "speciation_bio_ion", "speciation_biotransformation_ion",
                "ratio_gill", "ratio_bio", "ratio_bulk", "lyso_pump")
  default_vals <- c(0,0,0,1,1,1,0,0,0,1)
  g_data <- add_defaults_to_graph(g_data, defaults, default_vals)

  #close model
  g_data <- add_inputs_to_graph(g_data)

  return(g_data)
}
