test_that("Benchmarking BIONICv3 VBA to package", {

  #there is something weird with Q cba to change it for later

  input_df <- read.csv(testthat::test_path("fixtures", "bionicv3_input_test_data.csv"))
  #input_df <- input_df[input_df$ion_type!="Q",]
  rownames(input_df) <- 1:nrow(input_df)


  #catches error when one of the needed variables is not there
  error_df <- input_df[is.na(input_df$pka),]
  expect_warning(run_bionic_v3(error_df))

  input_df[input_df$ion_type=="N","pka"] <- 50

  #runs the model
  model_output_df <- run_bionic_v3(input_df)

  #gets the output data, removes ion types N and Q, and columns that are irrelevant
  output_df <- read.csv(testthat::test_path("fixtures", "bionicv3_output_test_data.csv"))
  #output_df <- output_df[output_df$ion_type!="Q",]
  output_df <- output_df[,!colnames(output_df)%in%c("ID","ion_type", "KOWI", "in_vitro_clearance",
                                                    "metabolic_oxygen", "gill_vent_rate")]
  rownames(output_df) <- 1:nrow(output_df)

  #matches the colnames
  model_output_df <- lapply(model_output_df, function(x)x[!sapply(x, is.list)])
  model_output_df <- do.call(dplyr::bind_rows, lapply(1:length(model_output_df), function(x)as.data.frame(model_output_df[[x]])))
  model_output_df <- model_output_df[,colnames(output_df)]
  for (i in colnames(model_output_df)){
    if(i%in%c("gill_elimination_rate", "fecal_egestion_rate", "growth_rate")){
      expect_equal(model_output_df[i], output_df[i], tolerance = 0.000001)
    }else{
      expect_equal(model_output_df[i], output_df[i])
    }
  }
})
