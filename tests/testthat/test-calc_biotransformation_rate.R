test_that("Test in vitro to in vivo clearance calculation",{
  #binary
  expect_equal(vitro_to_vivo_clearance(reaction_rate=0, assay_s9=0, liver_s9=0,
                                       liver_to_body_frac=0, in_vitro_clearance=0),0)

  expect_equal(vitro_to_vivo_clearance(1,1,1,1,1), 24)

  #check for changing between reaction rate and in_vitro clearance
  expect_equal(vitro_to_vivo_clearance(NA,1,1,2,3), 144)
  expect_equal(vitro_to_vivo_clearance(1,1,1,2,NA), 48)
  expect_error(vitro_to_vivo_clearance(NA,1,1,1,NA))

  #random example
  expect_equal(vitro_to_vivo_clearance(1.5, 2, 150, 0.1, NA), 270)

  #VBA
  #1
  expect_equal(vitro_to_vivo_clearance(0.5, 2, 163, 0.015, 0.25), 14.67)

  #9
  expect_equal(vitro_to_vivo_clearance(1.91552053981893, 1, 87.7551020408163,
                                       1.05331040412726E-02, NA), 42.4939208317529)


})

test_that("Test liver blood flow calculation", {
  #binary
  expect_error(calc_liver_blood_flow(biota_mass=0, water_temp=0, cardiac_output=0))
  expect_equal(calc_liver_blood_flow(1, 0, 0), 0)
  expect_equal(calc_liver_blood_flow(1, 1, 0), 0)
  expect_equal(calc_liver_blood_flow(1, 0, 1), -17.4663776)
  expect_equal(calc_liver_blood_flow(1, 1, 1), -12.31603549)

  #random example
  expect_equal(calc_liver_blood_flow(12, 25, 0.5), 43.40267638)

  #VBA code run
  #1
  expect_equal(calc_liver_blood_flow(0.01, 12, 0.259), 18.2000757891776)
})

test_that("Test the biotransformation constant rate calculation",{

  #div by 0 errors
  expect_error(calc_biotransformation_rate(liver_blood_flow=0, in_vivo_clearance=0, dfw_assay=0,
                                           dfw_blood_lysosome=0, dfw_biotransformation_lysosome=0))
  expect_error(calc_biotransformation_rate(0,0,1,1,1))
  expect_error(calc_biotransformation_rate(-1,1,1,1,1))
  expect_error(calc_biotransformation_rate(1,1,1,1,0))

  #binary inputs
  expect_equal(calc_biotransformation_rate(0,1,1,1,1), 0)
  expect_equal(calc_biotransformation_rate(1,1,0,1,1), 0)
  expect_equal(calc_biotransformation_rate(1,1,1,1,1), 0.5)

  #random example
  expect_equal(calc_biotransformation_rate(2, 10, 5, 2, 0.5), 7.407407407407407407)

  #vba example
  #3
  liver_blood_flow <- 18.2000757891776
  in_vivo_clearance <- 14.67
  dfw_assay <- 2.9817179042868
  dfw_blood_lysosome <- 682.755615031482
  dfw_biotransformation_lysosome <- 152.540327774899
  kb <-0.285749780146923
  expect_equal(calc_biotransformation_rate(liver_blood_flow, in_vivo_clearance, dfw_assay,
                                           dfw_blood_lysosome, dfw_biotransformation_lysosome), kb)

  #10
  liver_blood_flow <- 18.2000757891776
  in_vivo_clearance <- 10.3546178970451
  dfw_assay <- 10.1812340961762
  dfw_blood_lysosome <- 3648.14997453966
  dfw_biotransformation_lysosome <- 1617.02383021171
  kb<- 0.065092217340419
  expect_equal(calc_biotransformation_rate(liver_blood_flow, in_vivo_clearance, dfw_assay,
                                           dfw_blood_lysosome, dfw_biotransformation_lysosome), kb)

  #14
  liver_blood_flow <- 18.2000757891776
  in_vivo_clearance <- 1.467
  dfw_assay <- 2.30474556916181
  dfw_blood_lysosome <- 308.662096631372
  dfw_biotransformation_lysosome <- 82.175379074505
  kb <- 4.11197134827714E-02
  expect_equal(calc_biotransformation_rate(liver_blood_flow, in_vivo_clearance, dfw_assay,
                                           dfw_blood_lysosome, dfw_biotransformation_lysosome), kb)
})
