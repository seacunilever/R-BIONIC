library("readxl")

apply_partition <- function(df){

  #transformations to colnames to match with BIONICv3 packages vertices
  cols <- gsub(",", "", gsub("\\s+", "", colnames(df)))
  cols <- gsub("Δ", "delta_", cols)
  cols[grepl("delta", cols)] <- tolower(cols[grepl("delta", cols)])
  cols <- gsub("IOCTypeAcidBaseQuatorNeutral", "ion_type", cols)
  cols <- gsub("pKa", "pka", cols)
  colnames(df) <- cols

  #as the BIONICv3 does not use log 10, all logs are converted
  df_log <-10^(df[grepl("log", colnames(df))])
  colnames(df_log) <- gsub("log","",colnames(df_log))

  df <- cbind(df, df_log)

  return(df)
}

extract_input <- function(data_path, output_path){
  #Physical data sheet
  df_phys <- readxl::read_excel(data_path, sheet = "Phys-chem", skip = 3)

  df_phys <- apply_partition(df_phys)

  df_phys <- df_phys[!is.na(df_phys$ID),]

  #System data sheet
  df_system <- readxl::read_excel(data_path, sheet = "Test System", skip = 3)

  #transformations to colnames to match with BIONICv3 packages vertices
  colnames(df_system)[colnames(df_system)=="T (oC)"] <-"water_temp"
  cols_sys <- gsub("Bulk pH", "ph_bulk", colnames(df_system))
  colnames(df_system) <- cols_sys

  #Biota data sheet
  df_biota <- readxl::read_excel(data_path, sheet = "Biota", skip = 3)

  #transformations to colnames to match with BIONICv3 packages vertices
  colnames(df_biota)[5:ncol(df_biota)] <- c("biota_mass", "mass_frac_sl", "mass_frac_pl",
                                            "mass_frac_sa", "mass_frac_pw", "bio_ph",
                                            "ph_gill", "resistance_at_gill", "resistance_within_gill", "gamma_eff", "beta_eff",
                                            "KMWN_coeff_a", "KMWN_coeff_b")

  #IVIVE data sheet
  df_ivive <- readxl::read_excel(data_path, sheet = "IVIVE S9 --- BCF", skip = 3)

  #transformations to colnames to match with BIONICv3 packages vertices
  colnames(df_ivive)[5:ncol(df_ivive)] <- c("assay_mass_frac_sl", "assay_mass_frac_pl",
                                            "assay_mass_frac_sa", "assay_mass_frac_pw",
                                            "blood_mass_frac_sl", "blood_mass_frac_pl",
                                            "blood_mass_frac_sa", "blood_mass_frac_pw",
                                            "biotransformation_mass", "reaction_rate", "assay_s9",
                                            "liver_s9", "liver_to_body_frac", "cardiac_output")
  #merge all the data togethor
  df_input <- merge(merge(df_phys, df_biota), merge(df_system, df_ivive))

  #need to add dataframe of special consideration of lysosome sequestration
  df_spec <- data.frame("anionic_frac"=0.175,
                        "asorp_neut"= 1,
                        "asorp_acid"=0.05,
                        "asorp_base"=20,
                        "asorp_qac"=20,
                        "ph_lyso"=5.1,
                        "lyso_diam"=520,
                        "lyso_thick"=4,
                        "lyso_to_body_frac"= 0.001568,
                        "lyso_to_blood_frac"=0.0005)
  df_input <- cbind(df_input, df_spec)

  #the order is restored depending on ID
  df_input <- df_input[order(df_input[,"ID"]),]
  write.csv(df_input, output_path, row.names = F)
}

extract_output <- function(data_path, output_path){

  #gets bcf from sheet
  df_bcf <- readxl::read_excel(data_path, sheet = "Output - BCFs", skip = 3)

  df_bcf <- df_bcf[,c("ID", "Name", "CAS", "SMILES", "Default...8")]
  colnames(df_bcf)[5]<-"bcf"

  #gets the kinematic output
  df_kin <- readxl::read_excel(data_path, sheet = "Output - Kinetics", skip = 3)
  colnames(df_kin)[5:ncol(df_kin)] <- c("gill_uptake_efficiency", "gill_uptake_rate", "gill_elimination_rate",
                                        "biotransformation_rate", "fecal_egestion_rate", "growth_rate",
                                        "total_elimn_half_life", "TSS_95", "metabolic_oxygen",
                                        "metabolic_oxygen_mmol", "gill_vent_rate", "oxygen_conc_water")

  #gets the output partition and applies the partitioning steps to it
  df_part <- readxl::read_excel(data_path, sheet = "Output - Partitioning", skip = 3)
  df_part <- apply_partition(df_part)

  #gets the output ivive and renames it appropriatly
  df_output_ivive <- readxl::read_excel(data_path, sheet = "Output - IVIVE", skip = 3)
  rename <- c("biotransformation_mass", "reaction_rate", "assay_s9", "in_vitro_clearance", "in_vivo_clearance",
              "liver_blood_flow", "CLH", "VDSS", "biotransformation_rate", "QH_wrong")
  colnames(df_output_ivive)[5:ncol(df_output_ivive)] <- rename

  #merges the data and sorts it by ID
  df_output <- merge(merge(df_bcf, df_kin), merge(df_part, df_output_ivive))
  df_output <- df_output[order(df_output[,"ID"]),]

  #gets only the relevant
  rel <- c("ID","bcf", "gill_uptake_efficiency", "gill_uptake_rate", "gill_elimination_rate",
           "fecal_egestion_rate", "growth_rate", "biotransformation_rate", "metabolic_oxygen",
           "gill_vent_rate", "oxygen_conc_water", "ion_type", "pka", "delta_ow", "delta_sw",
           "delta_mw", "delta_saw", "delta_pw", "KOWN", "KOWI", "KSWN", "KSWI", "KMWN",
           "KMWI", "KSaWN", "KSaWI", "KPWI", "reaction_rate", "assay_s9", "in_vitro_clearance",
           "in_vivo_clearance", "liver_blood_flow")

  df_output <- df_output[,rel]
  write.csv(df_output, output_path, row.names = F)
}

data_path <- testthat::test_path("fixtures", "Bionicv3data.xlsm")
input_data_path <- testthat::test_path("fixtures", "bionicv3_input_test_data.csv")
output_data_path <- testthat::test_path("fixtures", "bionicv3_output_test_data.csv")


extract_input(data_path, input_data_path)
extract_output(data_path, output_data_path)
