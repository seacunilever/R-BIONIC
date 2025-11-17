test_that("Test for the calculations of undigest_list", {

  #binary
  diet_vol_frac_list <- list(sl = 0, pl = 0, sa = 0, pw = 0)
  density_list <- list(sl = 0, pl = 0, sa = 0, pw = 0)
  assimilation_eff_list <- list(l = 0, nlom = 0, w = 0)
  expect_error(calc_undigest_list(diet_vol_frac_list, density_list, assimilation_eff_list))

  #tests that none of the densities can be 0
  diet_vol_frac_list <- list(sl = 1, pl = 1, sa = 1, pw = 1)
  density_list <- list(sl = 1, pl = 1, sa = 1, pw = 0)
  assimilation_eff_list <- list(l = 0, nlom = 0, w = 0)
  expect_error(calc_undigest_list(diet_vol_frac_list, density_list, assimilation_eff_list))

  #example
  diet_vol_frac_list <- list(sl = 0.1, pl = 0.2, sa = 0.3, pw = 0.4)
  density_list <- list(sl = 1, pl = 1, sa = 1, pw = 1)
  assimilation_eff_list <- list(l = 0.1, nlom = 0.2, w = 0.3)
  undigest_list <- list(sl = 0.09, pl = 0.18, sa = 0.24, pw = 0.32, w = 0)
  expect_equal(calc_undigest_list(diet_vol_frac_list, density_list, assimilation_eff_list), undigest_list)

  #VBA example
  diet_vol_frac_list <- list(sl = 0.012, pl = 0.003, sa = 0, pw = 0.15)
  density_list <- list(sl = 0.9, pl = 1.015, sa = 1.36, pw = 1)
  assimilation_eff_list <- list(l = 0.92, nlom = 0.6, w = 0.7)
  undigest_list <- list(sl = 1.06666666666667E-03, pl = 2.36453201970443E-04, sa = 0, pw = 0.06,
             w = 0.250113300492611)
  expect_equal(calc_undigest_list(diet_vol_frac_list, density_list, assimilation_eff_list), undigest_list)

})

test_that("Test for calculations for fecal volume fraction list", {
  undigest_list <- list(sl = 0, pl = 0, sa = 0, pw = 0, w = 0)
  expect_error(calc_fecal_vol_frac_list(undigest_list))

  undigest_list <- list(sl = 1, pl = 0, sa = 0, pw = 0, w = 0)
  expect_equal(calc_fecal_vol_frac_list(undigest_list),undigest_list)

  undigest_list <- list(sl = 1, pl = 1, sa = 1, pw = 1, w = 0)
  fecal_vol_frac_list <- list(sl = 0.25, pl = 0.25, sa = 0.25, pw = 0.25, w = 0)
  expect_equal(calc_fecal_vol_frac_list(undigest_list), fecal_vol_frac_list)

  #random example
  undigest_list <- list(sl = 0.1, pl = 0.22, sa = 0.04, pw = 0.3 , w = 0)
  fecal_vol_frac_list <- list(sl = 0.1/0.66, pl = 1/3, sa = 2/33, pw = 5/11, w = 0)
  expect_equal(calc_fecal_vol_frac_list(undigest_list), fecal_vol_frac_list)

  #VBA run test
  undigest_list <- list(sl = 1.06666666666667E-03, pl = 2.36453201970443E-04, sa = 0, pw = 0.06, w = 0.250113300492611)
  fecal_vol_frac_list <- list(sl = 3.42521009466783E-03, pl = 7.59283025911587E-04,
                                sa = 0, pw = 0.192668067825065, w = 0.803147439054355)
  expect_equal(calc_fecal_vol_frac_list(undigest_list), fecal_vol_frac_list)


})

test_that("Test for diet_rate calculation",{
  expect_equal(calc_diet_rate(biota_mass = 0, water_temp = 0), 0)
  expect_equal(calc_diet_rate(1,0), 0.022)
  expect_equal(calc_diet_rate(0,1), 0)
  expect_equal(calc_diet_rate(1,1), 0.02336040402)

  #random example
  expect_equal(calc_diet_rate(0.5, 25), 0.05470019913)

  #vba examples
  #7
  expect_equal(calc_diet_rate(0.01, 12), 9.0180929604002E-04)
})

test_that("Test for calculating the fecal rate", {

  #binary
  undigest_list <- list(sl = 0, pl = 0, sa = 0, pw = 0, w = 0)
  expect_equal(calc_fecal_rate(undigest_list, diet_rate=0),0)

  expect_equal(calc_fecal_rate(undigest_list, 1),0)

  undigest_list <- list(sl = 1, pl = 0, sa = 0, pw = 0, w = 0)
  expect_equal(calc_fecal_rate(undigest_list, 1),1)

  undigest_list <- list(sl = 1, pl = 1, sa = 1, pw = 1, w = 0)
  expect_equal(calc_fecal_rate(undigest_list, 1),4)

  #random example
  undigest_list <- list(sl = 0.1, pl = 0.22, sa = 0.04, pw = 0.3, w = 0)
  expect_equal(calc_fecal_rate(undigest_list, 24), 15.84)

  #VBA example
  undigest_list <- list(sl = 1.06666666666667E-03, pl = 2.36453201970443E-04, sa = 0, pw = 0.06, w = 0.250113300492611)
  expect_equal(calc_fecal_rate(undigest_list,9.0180929604002E-04), 2.8083822282128E-04)
})

test_that("Test for the dietary uptake calculation", {
  #binary
  partition_list <- list(KOWN=0, KMWI=0)
  expect_error(calc_dietary_uptake(partition_list, ion_type = "A", dietary_uptake_a = 0, dietary_uptake_b = 0))

  expect_equal(calc_dietary_uptake(partition_list, ion_type = "A", 0, 1), 1)
  partition_list <- list(KOWN=1, KMWI=0)
  expect_equal(calc_dietary_uptake(partition_list, ion_type = "A", 1, 1), 0.5)

  #test for different ion types
  partition_list <- list(KOWN=1, KMWI=2)
  expect_equal(calc_dietary_uptake(partition_list, ion_type = "A", 1, 0), 1)
  expect_equal(calc_dietary_uptake(partition_list, ion_type = "B", 1, 0), 1)
  expect_equal(calc_dietary_uptake(partition_list, ion_type = "N", 1, 0), 1)
  expect_equal(calc_dietary_uptake(partition_list, ion_type = "Q", 1, 0), 0.5)

  #random example
  partition_list <- list(KOWN=550, KMWI=10342)
  expect_equal(calc_dietary_uptake(partition_list, ion_type = "A", 1, 450), 1/1000)

  #vba example
  #5
  partition_list <- list(KOWN=316227.766016838, KMWI=47315.125896148)
  ion_type <- "B"
  ed_a <- 0.000000053
  ed_b <- 2
  ed <- 0.495844802801554
  expect_equal(calc_dietary_uptake(partition_list, ion_type, ed_a, ed_b), ed)

  #8
  partition_list <- list(KOWN=0.0000000001, KMWI=31622.7766016838)
  ion_type <- "Q"
  ed_a <- 0.000000053
  ed_b <- 2
  ed <- 0.49958134904103
  expect_equal(calc_dietary_uptake(partition_list, ion_type, ed_a, ed_b), ed)

  #10
  partition_list <- list(KOWN=6760829.75391982, KMWI=89125.0938133746)
  ion_type <- "A"
  ed_a <- 0.000000053
  ed_b <- 2
  ed <- 0.424029950833984
  expect_equal(calc_dietary_uptake(partition_list, ion_type, ed_a, ed_b), ed)
})

test_that("Test for the git partition calculation",{

  #binary
  fecal_vol_frac_list <- list(sl=0, pl=0, sa=0, pw=0, w=0)
  distribution_ratio_list <- list(DSW=0, DMW=0, DSAW=0, DPW=0)
  expect_error(calc_git_partition(fecal_vol_frac_list, distribution_ratio_list, dfw_uptake_lysosome=0))

  expect_equal(calc_git_partition(fecal_vol_frac_list, distribution_ratio_list, 1), 0)

  fecal_vol_frac_list <- list(sl=1, pl=0, sa=0, pw=0, w=0)
  distribution_ratio_list <- list(DSW=1, DMW=0, DSAW=0, DPW=0)
  expect_equal(calc_git_partition(fecal_vol_frac_list, distribution_ratio_list, 1), 1)

  fecal_vol_frac_list <- list(sl=1, pl=1, sa=1, pw=1, w=1)
  distribution_ratio_list <- list(DSW=1, DMW=1, DSAW=1, DPW=1)
  expect_equal(calc_git_partition(fecal_vol_frac_list, distribution_ratio_list, 1), 5)

  #VBA example
  #8
  fecal_vol_frac_list <- list(sl=3.42521009466783E-03, pl=7.59283025911587E-04,
                               sa=0, pw=0.192668067825065, w=0.803147439054355)
  distribution_ratio_list <- list(DSW=0.0000000000001, DMW=31622.7766016838, DSAW=30.5492111321551,
                                 DPW=3.31131121482591)
  dfw <- 1687.93066352313
  KGB <- 1.50786815055014E-02

  expect_equal(calc_git_partition(fecal_vol_frac_list, distribution_ratio_list, dfw), KGB)

  #15
  fecal_vol_frac_list <- list(sl=3.42521009466783E-03, pl=7.59283025911587E-04,
                               sa=0, pw=0.192668067825065, w=0.803147439054355)
  distribution_ratio_list <- list(DSW=323.655254758577, DMW=4897.86854346647, DSAW=21483.2508534137,
                                 DPW=5.34666116657249)
  dfw <- 112.73197605778
  KGB <- 5.90846913382503E-02

  expect_equal(calc_git_partition(fecal_vol_frac_list, distribution_ratio_list, dfw), KGB)
})

test_that("Test for the fecal egestion rate calculation", {

  #binary examples
  expect_error(calc_fecal_egestion_rate(biota_mass=0, git_partition=0, dietary_uptake=0,
                                        fecal_rate=0))
  expect_equal(calc_fecal_egestion_rate(1,0,0,0),0)
  expect_equal(calc_fecal_egestion_rate(1,1,0,0),0)
  expect_equal(calc_fecal_egestion_rate(1,0,1,0),0)
  expect_equal(calc_fecal_egestion_rate(1,0,0,1),0)
  expect_equal(calc_fecal_egestion_rate(1,1,1,1),1)

  #random example
  expect_equal(calc_fecal_egestion_rate(0.258, 2.5, 0.75, 3.53), 25.65406977)

  #VBA example

  #5
  biota_mass <- 0.01
  KGB <- 8.09156049234229E-03
  ED <- 0.495844802801554
  GF <- 2.8083822282128E-04
  kf <- 1.12676738325085E-04
  expect_equal(calc_fecal_egestion_rate(biota_mass, KGB, ED, GF), kf)

  #11
  biota_mass <- 0.01
  KGB <- 0.062857943026686
  ED <- 0.444859721017302
  GF <- 2.8083822282128E-04
  kf <- 7.85306995668934E-04
  expect_equal(calc_fecal_egestion_rate(biota_mass, KGB, ED, GF), kf)
})
