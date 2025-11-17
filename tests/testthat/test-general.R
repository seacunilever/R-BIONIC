test_that("Paritition neutral works", {
  #binary inputs - this is where log 10 of 0 comes in
  expect_error(calc_partition_neut(0,0,0))
  expect_equal(calc_partition_neut(1,0,0), 1)
  expect_error(calc_partition_neut(0,1,0))
  expect_error(calc_partition_neut(0,0,1))

  #calculated example
  expect_equal(calc_partition_neut(1,1,1), 10)
  expect_equal(calc_partition_neut(100, 1,2.5), 31622.7766)

  #examples from VBA
  KOWN <- 316227.766016838
  expect_equal(calc_partition_neut(KOWN, 1.01, 0.12), 473151.25896148)
  expect_equal(calc_partition_neut(KOWN, 0.8, -0.68), 5248.07460249773)
  expect_equal(calc_partition_neut(KOWN, 1, 0), KOWN)

  KOWN <- 0.0000000001
  expect_equal(calc_partition_neut(KOWN, 1.01, 0.12), 1.0471285480509E-10)
  expect_equal(calc_partition_neut(KOWN, 0.8, -0.68), 2.08929613085404E-09)
  expect_equal(calc_partition_neut(KOWN, 1, 0), KOWN)
})

test_that("using the scaling factor works", {
  #binary inputs
  expect_equal(calc_partition_ion(0, 0), 0)
  expect_equal(calc_partition_ion(1, 0), 1)
  expect_equal(calc_partition_ion(0, 1), 0)

  #calculated example
  expect_equal(calc_partition_ion(10, -2), 1000)
  expect_equal(calc_partition_ion(25, 3.5), 0.00790569415)

  #examples from VBA
  KPWN <- 5248.07460249773
  KMWN <- 473151.25896148

  expect_equal(calc_partition_ion(KPWN, 3), 5.24807460249773)
  expect_equal(calc_partition_ion(KMWN, 2), 4731.5125896148)
  expect_equal(calc_partition_ion(KMWN, 1), 47315.125896148)

})

test_that("Test for calculating KSWaN", {

  #calculated examples
  expect_error(calc_partition_sawn(0))
  expect_equal(calc_partition_sawn(1), 0.1995262315)
  expect_equal(calc_partition_sawn(10^4.5), 16788.04018)
  expect_equal(calc_partition_sawn(10^10), 1819700.859)

  #examples from VBA
  KOWN <- 316227.766016838
  expect_equal(calc_partition_sawn(KOWN), 39355.0075455777)

  KOWN <- 0.0000000001
  expect_equal(calc_partition_sawn(KOWN), 3.16227766016838E-12)
})

test_that("reverse of KOWN works", {
  #binary inputs - this is where log 10 of 0 comes in
  expect_error(calc_oct_water_neut(0,0,0))
  expect_error(calc_oct_water_neut(1,0,0))
  expect_error(calc_oct_water_neut(0,1,0))
  expect_error(calc_oct_water_neut(0,0,1))

  #calculated example
  expect_equal(calc_oct_water_neut(10,1,1), 1)
  expect_equal(calc_oct_water_neut(31622.7766, 1,2.5), 100)

  #examples from VBA
  KMWN <- 473151.25896148
  expect_equal(calc_oct_water_neut(KMWN, 1.01, 0.12), 316227.766016838)

  KMWN <- 1.0471285480509E-10
  expect_equal(calc_oct_water_neut(KMWN, 1.01, 0.12), 0.0000000001)
})

test_that("RATIO calculation works", {

  #tests that ION types works
  expect_equal(calc_ratio(2, 1, "A"), 10)
  expect_equal(calc_ratio(2, 1, "N"), 10)
  expect_equal(calc_ratio(2, 1, "B"), 0.1)
  expect_equal(calc_ratio(2, 1, "Q"), 10)

  #Zeros
  expect_equal(calc_ratio(0, 0, "A"), 1)
  expect_equal(calc_ratio(0, 0, "B"), 1)

  #random
  expect_equal(calc_ratio(1, 3.5, "A"), 0.00316227766)
  expect_equal(calc_ratio(14, 11, "B"), 0.001)

  #from VBA
  #bio
  expect_equal(calc_ratio(7.8, 3.9, "A"), 7943.28234724281)
  expect_equal(calc_ratio(7.8, 10.1, "B"), 199.526231496888)
  #gill
  expect_equal(calc_ratio(6.4659, 3.9, "A"), 368.044218571993)
  expect_equal(calc_ratio(6.4659, 10.1, "B"), 4306.25754321173)
  #bulk
  expect_equal(calc_ratio(7, 3.9, "A"), 1258.92541179417)
  expect_equal(calc_ratio(7, 10.1, "B"), 1258.92541179417)
})

test_that("Calculate the specitiation neutral form", {
  expect_equal(calc_speciation_neut(0), 1)
  expect_equal(calc_speciation_neut(1), 0.5)
  expect_error(calc_speciation_neut(-1))
  expect_error(calc_speciation_neut(-0.00000001))

  #random
  expect_equal(calc_speciation_neut(24), 0.04)

  #From VBA
  ratio <- 6.30957344480189E-43
  expect_equal(calc_speciation_neut(ratio), 1)

  ratio <- 4365158.32240166
  expect_equal(calc_speciation_neut(ratio), 2.29086712796043E-07)
})

test_that("Calculate the specitiation ion form", {
  expect_equal(calc_speciation_ion(0), 1)
  expect_equal(calc_speciation_ion(1), 0)
  expect_error(calc_speciation_ion(-1))
  expect_error(calc_speciation_ion(3))

  #random
  expect_equal(calc_speciation_ion(0.37), 0.63)

  #From VBA
  ratio <- 1
  expect_equal(calc_speciation_ion(ratio), 0)

  ratio <- 2.29086712796043E-07
  expect_equal(calc_speciation_ion(ratio), 0.999999770913287)
})

test_that("Test to see if distribution ratio calculation works", {

  #binary inputs
  expect_equal(calc_distribution_ratio(0, 0, 0, 0), 0)
  expect_equal(calc_distribution_ratio(1, 0, 0, 0), 0)
  expect_equal(calc_distribution_ratio(0, 1, 0, 0), 0)
  expect_equal(calc_distribution_ratio(0, 0, 1, 0), 0)
  expect_equal(calc_distribution_ratio(0, 0, 0, 1), 0)
  expect_equal(calc_distribution_ratio(1, 0, 1, 0), 1)
  expect_equal(calc_distribution_ratio(0, 1, 0, 1), 1)
  expect_equal(calc_distribution_ratio(1, 1, 1, 1), 2)

  #From VBA
  #4
  KSWN <- 316227.766016838
  KSWI <- 316.227766016838
  FN_bio <- 1.25876694242503E-04
  FI_bio <-0.999874123305758
  DSW <- 355.993666124915
  expect_equal(calc_distribution_ratio(KSWN, KSWI, FN_bio, FI_bio), DSW)

  #4
  KMWN <- 473151.25896148
  KMWI <- 4731.5125896148
  FN_bio <- 1.25876694242503E-04
  FI_bio <-0.999874123305758
  DMW <- 4790.47571880601
  expect_equal(calc_distribution_ratio(KMWN, KMWI, FN_bio, FI_bio), DMW)

  #13
  KSWN <- 63679552.0907916
  KSWI <- 63679.5520907916
  FN_bio <- 1.90546035488526E-07
  FI_bio <- 0.999999809453964
  DSW <- 63691.673843098
  expect_equal(calc_distribution_ratio(KSWN, KSWI, FN_bio, FI_bio), DSW)
})

test_that("Test to see if DMWaN calculation works", {

  #binary inputs
  expect_equal(calc_distribution_ratio_mwan(0, 0, 0, 0, 0), 0)
  expect_equal(calc_distribution_ratio_mwan(1, 0, 0, 0, 0), 0)
  expect_equal(calc_distribution_ratio_mwan(0, 1, 0, 0, 0), 0)
  expect_equal(calc_distribution_ratio_mwan(0, 0, 1, 0, 0), 0)
  expect_equal(calc_distribution_ratio_mwan(0, 0, 0, 1, 0), 0)
  expect_equal(calc_distribution_ratio_mwan(1, 0, 1, 0, 0), 1)
  expect_equal(calc_distribution_ratio_mwan(0, 1, 0, 1, 0), 0)
  expect_equal(calc_distribution_ratio_mwan(1, 1, 1, 1, 0), 1)
  expect_equal(calc_distribution_ratio_mwan(1, 1, 1, 1, 1), 2)

  #4
  KMWN <- 473151.25896148
  KMWI <- 4731.5125896148
  FN_bio <- 1.25876694242503E-04
  FI_bio <- 0.999874123305758
  asorp_chem <- 0.05
  DMWan <- 296.104566477313
  expect_equal(calc_distribution_ratio_mwan(KMWN, KMWI, FN_bio, FI_bio, asorp_chem), DMWan)
})

#could add test for volume fraction equation but I think thats overkill

test_that("Test to see if distribution ratio of fish water works", {

  vol_frac_list <- list(sl=0, sa=0, pw=0, pl=0, water=0)
  distribution_ratio_list <- list(DSW=0, DSAW=0, DPW=0, DMW=0, DMWAN=0)
  anionic_frac <- 0
  expect_equal(calc_dfw(vol_frac_list, distribution_ratio_list, anionic_frac), 0)

  vol_frac_list <- list(sl=1, sa=1, pw=1, pl=1, water=1)
  distribution_ratio_list <- list(DSW=0, DSAW=0, DPW=0, DMW=0, DMWAN=0)
  anionic_frac <- 1
  expect_equal(calc_dfw(vol_frac_list, distribution_ratio_list, anionic_frac), 1)

  vol_frac_list <- list(sl=0, sa=0, pw=0, pl=0, water=0)
  distribution_ratio_list <- list(DSW=1, DSAW=1, DPW=1, DMW=1, DMWAN=1)
  anionic_frac <- 1
  expect_equal(calc_dfw(vol_frac_list, distribution_ratio_list, anionic_frac), 0)

  vol_frac_list <- list(sl=1, sa=1, pw=1, pl=1, water=1)
  distribution_ratio_list <- list(DSW=1, DSAW=1, DPW=1, DMW=1, DMWAN=1)
  anionic_frac <- 1
  expect_equal(calc_dfw(vol_frac_list, distribution_ratio_list, anionic_frac), 5)

  #random
  vol_frac_list <- list(sl=0.1, sa=0.2, pw=0.3, pl=0.05, water=0.35)
  distribution_ratio_list <- list(DSW=23, DSAW=12, DPW=32, DMW=0.3, DMWAN=5)
  anionic_frac <- 0.175
  expect_equal(calc_dfw(vol_frac_list, distribution_ratio_list, anionic_frac), 14.706125)

  #from VBA
  #6
  vol_frac_list <- list(sl=4.16666666666667E-02, sa=2.20588235294118E-03, pw=0.15,
                        pl=1.23152709359606E-02, water=0.793812180044432)

  distribution_ratio_list <- list(DSW=1891.64029879425, DSAW=4112.13353983789, DPW=31.3934147346021,
                                  DMW=49438.719053445, DMWAN=943942.969970408)
  anionic_frac <- 0.175
  expect_equal(calc_dfw(vol_frac_list, distribution_ratio_list, anionic_frac), 2630.05415844871)
})

test_that("Test to see ph_gill calculation works", {

  expect_equal(calc_ph_gill(0), 1.4672)
  expect_equal(calc_ph_gill(1), 2.1813)
  expect_equal(calc_ph_gill(2.35), 3.145335)

  #from VBA
  expect_equal(calc_ph_gill(7), 6.4659)

})

#could add test for asorp_chem, delta_from_ow and delta_from_ion equations but I think thats overkill
