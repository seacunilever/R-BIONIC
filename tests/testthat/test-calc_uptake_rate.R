# I split up the uptake efficiency as its quite a big function for more transparency
test_that("Test calculate uptake efficiency - binary inputs", {
  #binary inputs
  partition_list <- list(KMWN =0, KMWI =0)

  expect_error(calc_gill_uptake_efficiency(partition_list, ratio_gill=0, ion_type="A",
                                      speciation_gill_neut=0, speciation_gill_ion=0,
                                      resistance_at_gill=0, resistance_within_gill=0, z_water_value=0, gamma_eff=0, beta_eff=0))
  #left side of transport
  partition_list <- list(KMWN=1, KMWI=0)
  expect_equal(calc_gill_uptake_efficiency(partition_list, ratio_gill=0, ion_type="A",
                                      speciation_gill_neut=1, speciation_gill_ion=0,
                                      resistance_at_gill=1, resistance_within_gill=1, z_water_value=1, gamma_eff=1, beta_eff=0), 0.5)

  #right side of transport
  partition_list <- list(KMWN=0, KMWI=1)
  expect_equal(calc_gill_uptake_efficiency(partition_list, ratio_gill=1, ion_type="A",
                                      speciation_gill_neut=0, speciation_gill_ion=1,
                                      resistance_at_gill=1, resistance_within_gill=1, z_water_value=1, gamma_eff=1, beta_eff=0), 1/3)
  #without resistance_at_gill
  partition_list <- list(KMWN=1, KMWI=0)
  expect_equal(calc_gill_uptake_efficiency(partition_list, ratio_gill=0, ion_type="A",
                                      speciation_gill_neut=1, speciation_gill_ion=0,
                                      resistance_at_gill=0, resistance_within_gill=1, z_water_value=1, gamma_eff=1, beta_eff=0), 1)

  #all 1
  partition_list <- list(KMWN=1, KMWI=1)
  expect_equal(calc_gill_uptake_efficiency(partition_list, ratio_gill=1, ion_type="A",
                                      speciation_gill_neut=1, speciation_gill_ion=1,
                                      resistance_at_gill=1, resistance_within_gill=1, z_water_value=1, gamma_eff=1, beta_eff=1), 1.5)
})

test_that("Test calculate uptake efficiency - beta",{

  partition_list <- list(KMWN=1, KMWI=0)
  #beta checker
  expect_equal(calc_gill_uptake_efficiency(partition_list, ratio_gill=0, ion_type="A",
                                      speciation_gill_neut=1, speciation_gill_ion=0,
                                      resistance_at_gill=0, resistance_within_gill=1, z_water_value=1, gamma_eff=1, beta_eff=0.012), 1.012)
})

test_that("Test calculate uptake efficiency - gamma checker",{
  #gamma checker
  partition_list <- list(KMWN=0, KMWI=1)
  expect_equal(calc_gill_uptake_efficiency(partition_list, ratio_gill=1, ion_type="A",
                                      speciation_gill_neut=0, speciation_gill_ion=1,
                                      resistance_at_gill=0, resistance_within_gill=1, z_water_value=1, gamma_eff=1, beta_eff=0), 0.5)

  #looks at gamma NA
  expect_equal(calc_gill_uptake_efficiency(partition_list, ratio_gill=1, ion_type="A",
                                      speciation_gill_neut=0, speciation_gill_ion=1,
                                      resistance_at_gill=0, resistance_within_gill=1, z_water_value=1, gamma_eff=NA, beta_eff=0), 1/20000)

  #looks at basics
  expect_equal(calc_gill_uptake_efficiency(partition_list, ratio_gill=1, ion_type="B",
                                      speciation_gill_neut=0, speciation_gill_ion=1,
                                      resistance_at_gill=0, resistance_within_gill=1, z_water_value=1, gamma_eff=NA, beta_eff=0), 1/600000)

  #looks at neutrals
  expect_equal(calc_gill_uptake_efficiency(partition_list, ratio_gill=1, ion_type="N",
                                      speciation_gill_neut=0, speciation_gill_ion=1,
                                      resistance_at_gill=0, resistance_within_gill=1, z_water_value=1, gamma_eff=NA, beta_eff=0), 1/200)
})

test_that("Test calculate uptake efficiency - Q checker",{
  #Q checker
  partition_list <- list(KMWN=1, KMWI=1)
  expect_equal(calc_gill_uptake_efficiency(partition_list, ratio_gill=1, ion_type="Q",
                                      speciation_gill_neut=1, speciation_gill_ion=1,
                                      resistance_at_gill=0, resistance_within_gill=1, z_water_value=1, gamma_eff=1, beta_eff=0), 1)
})

test_that("Test calculate uptake efficiency - VBA runs", {

  #VBA test
  #4
  partition_list <- list(KMWN=473151.25896148, KMWI=4731.5125896148)
  ratio_gill <- 368.044218571993
  ion_type <- "A"
  speciation_gill_neut <- 2.70970238707295E-03
  speciation_gill_ion <- 0.997290297612927
  resistance_at_gill <- 1.85
  resistance_within_gill <- 155
  z_water_value <- 1
  gamma_eff <- NA
  beta_eff <- 0.0001
  uptake_ef <- 2.44053213238697E-02

  expect_equal(calc_gill_uptake_efficiency(partition_list, ratio_gill, ion_type,
                                      speciation_gill_neut, speciation_gill_ion,
                                      resistance_at_gill, resistance_within_gill, z_water_value, gamma_eff, beta_eff), uptake_ef)

  #13
  partition_list <- list(KMWN=83945998.6519398, KMWI=831763.771102671)
  ratio_gill <- 243164.403787591
  ion_type <- "A"
  speciation_gill_neut <- 4.11242711513977E-06
  speciation_gill_ion <- 0.999995887572885
  resistance_at_gill <- 1.85
  resistance_within_gill <- 155
  z_water_value <- 1
  gamma_eff <- NA
  beta_eff <- 0.0001
  uptake_ef <- 0.269388213022821

  expect_equal(calc_gill_uptake_efficiency(partition_list, ratio_gill, ion_type,
                                      speciation_gill_neut, speciation_gill_ion,
                                      resistance_at_gill, resistance_within_gill, z_water_value, gamma_eff, beta_eff), uptake_ef)
})

test_that("Test oxygen concentraiton calculations", {

  #binary examples
  expect_equal(calc_oxygen_conc_water(water_temp=0, saturation=0), 0)
  expect_equal(calc_oxygen_conc_water(1,0), 0)
  expect_equal(calc_oxygen_conc_water(0,1), 14.45)
  expect_equal(calc_oxygen_conc_water(1,1), 14.04256)

  #random example
  expect_equal(calc_oxygen_conc_water(10, 0.85), 9.2446)

  #VBA example
  expect_equal(calc_oxygen_conc_water(12, 0.9), 9.265176)
})

test_that("Test metabolic oxygen requirments", {
  #binary
  expect_error(calc_metabolic_oxygen(0, 0))
  expect_equal(calc_metabolic_oxygen(1, 0), 630.95734448019324943436013662234)
  expect_equal(calc_metabolic_oxygen(1, 1), 656.14526630290540489111294700492)

  #random example
  expect_equal(calc_metabolic_oxygen(0.25, 25), 564.65196887919355304659619161336)
})

test_that("Test the gill ventilation rate calculation", {
  expect_error(calc_gill_vent_rate(oxygen_conc_water=0, metabolic_oxygen=0, eff_oxy_transfer=0))
  expect_equal(calc_gill_vent_rate(1,0,1), 0)
  expect_equal(calc_gill_vent_rate(1,1,1), 1)

  #random example
  expect_equal(calc_gill_vent_rate(10, 500, 0.5), 100)

  #VBA example
  oxygen_conc_water <- 9.265176
  metabolic_oxy <- calc_metabolic_oxygen(biota_mass=0.01, water_temp = 12)
  expect_equal(calc_gill_vent_rate(oxygen_conc_water, metabolic_oxy, 0.65), 4.48986176278015)

})

test_that("Test the calculation of the uptake rate", {

  #binary tests
  expect_error(calc_gill_uptake_rate(biota_mass=0, gill_vent_rate=0, gill_uptake_efficiency=0))
  expect_equal(calc_gill_uptake_rate(1,0,0),0)
  expect_equal(calc_gill_uptake_rate(1,1,0),0)
  expect_equal(calc_gill_uptake_rate(1,0,1),0)
  expect_equal(calc_gill_uptake_rate(1,1,1),1)

  #random example
  expect_equal(calc_gill_uptake_rate(0.25,100,0.2),80)

  #VBA example
  #1
  expect_equal(calc_gill_uptake_rate(0.01, 4.48986176278015, 0.540544840671164), 242.697161119755)
  #8
  expect_equal(calc_gill_uptake_rate(0.01, 4.48986176278015, 7.79205196696997E-04), 0.349852361800944)

})
