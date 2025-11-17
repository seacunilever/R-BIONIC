test_that("Test calculate for BCF", {
  vol_frac_list <- list(water = 0)
  expect_error(calc_bcf(gill_uptake_rate = 0, gill_elimination_rate = 0, growth_rate = 0,
                        fecal_egestion_rate = 0, biotransformation_rate = 0, vol_frac_list))

  expect_equal(calc_bcf(gill_uptake_rate = 0, gill_elimination_rate = 1, growth_rate = 0,
                        fecal_egestion_rate = 0, biotransformation_rate = 0, vol_frac_list), 0)

  expect_equal(calc_bcf(gill_uptake_rate = 1, gill_elimination_rate = 0, growth_rate = 1,
                        fecal_egestion_rate = 0, biotransformation_rate = 0, vol_frac_list), 1)

  expect_equal(calc_bcf(gill_uptake_rate = 1, gill_elimination_rate = 1, growth_rate = 1,
                        fecal_egestion_rate = 1, biotransformation_rate = 1, vol_frac_list), 0.25)

  vol_frac_list <- list(water = 0.1)
  expect_equal(calc_bcf(gill_uptake_rate = 1, gill_elimination_rate = 1, growth_rate = 1,
                        fecal_egestion_rate = 1, biotransformation_rate = 1, vol_frac_list), 0.35)

  #random example
  vol_frac_list <- list(water = 0.1)
  expect_equal(calc_bcf(gill_uptake_rate = 1.5, gill_elimination_rate = 0.1, growth_rate = 0.1,
                        fecal_egestion_rate = 0.1, biotransformation_rate = 0.2, vol_frac_list), 3.1)

  #VBA example
  #3
  vol_frac_list <- list(water = 0.793812180044432)
  expect_equal(calc_bcf(gill_uptake_rate = 10.9576519020406, gill_elimination_rate = 1.13926036639619E-02,
                        growth_rate = 1.57013249362404E-03, fecal_egestion_rate = 6.2059149177757E-04,
                        biotransformation_rate = 0.285749780146923, vol_frac_list), 37.4006946686259)

  #9
  vol_frac_list <- list(water = 0.793812180044432)
  expect_equal(calc_bcf(gill_uptake_rate = 7.43211007183882, gill_elimination_rate = 2.22318389553229E-03,
                        growth_rate = 1.57013249362404E-03, fecal_egestion_rate = 7.67745774944791E-04,
                        biotransformation_rate = 0.295929269244414, vol_frac_list), 25.5270874138985)

  #13
  vol_frac_list <- list(water = 0.793812180044432)
  expect_equal(calc_bcf(gill_uptake_rate = 120.951583699484, gill_elimination_rate = 1.44460987577616E-03,
                        growth_rate = 1.57013249362404E-03, fecal_egestion_rate = 3.62639488488505E-04,
                        biotransformation_rate = 0.10333595396873, vol_frac_list), 1134.21900934564)
})
