test_that("Test gill elmination rate with binary values", {

  #checks error of divions by 0
  expect_error(calc_gill_elimination_rate(gill_uptake_rate=0, dfw_uptake_lysosome=0, ratio_bio=0,
                                          ratio_bulk=0, ion_type="A", z_water_value=0))

  expect_error(calc_gill_elimination_rate(gill_uptake_rate=0, dfw_uptake_lysosome=1, ratio_bio=-1,
                                          ratio_bulk=1, ion_type="A", z_water_value=1))

  #binary_checks
  expect_equal(calc_gill_elimination_rate(gill_uptake_rate=0, dfw_uptake_lysosome=1, ratio_bio=0,
                                          ratio_bulk=0, ion_type="A", z_water_value=1), 0)

  expect_equal(calc_gill_elimination_rate(gill_uptake_rate=1, dfw_uptake_lysosome=1, ratio_bio=0,
                                          ratio_bulk=0, ion_type="A", z_water_value=1), 1)

  expect_equal(calc_gill_elimination_rate(gill_uptake_rate=1, dfw_uptake_lysosome=1, ratio_bio=1,
                                          ratio_bulk=0, ion_type="A", z_water_value=1), 0.5)

  expect_equal(calc_gill_elimination_rate(gill_uptake_rate=1, dfw_uptake_lysosome=1, ratio_bio=0,
                                          ratio_bulk=1, ion_type="A", z_water_value=1), 2)

  #all 1s
  expect_equal(calc_gill_elimination_rate(gill_uptake_rate=1, dfw_uptake_lysosome=1, ratio_bio=1,
                                          ratio_bulk=1, ion_type="A", z_water_value=1), 1)
})

test_that("Test gill elmination rate with chaning ion types",{
  expect_equal(calc_gill_elimination_rate(gill_uptake_rate=1, dfw_uptake_lysosome=1, ratio_bio=1,
                                          ratio_bulk=0, ion_type="B", z_water_value=1), 0.5)
  expect_equal(calc_gill_elimination_rate(gill_uptake_rate=1, dfw_uptake_lysosome=1, ratio_bio=1,
                                          ratio_bulk=0, ion_type="N", z_water_value=1), 0.5)
  expect_equal(calc_gill_elimination_rate(gill_uptake_rate=1, dfw_uptake_lysosome=1, ratio_bio=1,
                                          ratio_bulk=0, ion_type="Q", z_water_value=1), 1)
})

test_that("Test gill elimination rate calulation from VBA",{

  #1
  gill_uptake_rate <- 242.697161119755
  dfw_uptake_lysosome <- 19863.6520250665
  ratio_bio <- 6.30957344480189E-43
  ratio_bulk <- 1E-43
  ion_type <- "N"
  kw <- 1.22181540843289E-02
  expect_equal(calc_gill_elimination_rate(gill_uptake_rate, dfw_uptake_lysosome, ratio_bio, ratio_bulk,
                                          ion_type, z_water_value=1), kw)

  #8
  gill_uptake_rate <- 0.349852361800944
  dfw_uptake_lysosome <- 1687.93066352313
  ratio_bio <- 0
  ratio_bulk <- 0
  ion_type <- "Q"
  kw <- 2.07267021899297E-04
  expect_equal(calc_gill_elimination_rate(gill_uptake_rate, dfw_uptake_lysosome, ratio_bio, ratio_bulk,
                                          ion_type, z_water_value=1), kw)

  #12
  gill_uptake_rate <- 57.0217742572718
  dfw_uptake_lysosome <- 4344.73156456123
  ratio_bio <- 5248074.60249772
  ratio_bulk <- 831763.771102671
  ion_type <- "A"
  kw <- 2.08007127556485E-03
  expect_equal(calc_gill_elimination_rate(gill_uptake_rate, dfw_uptake_lysosome, ratio_bio, ratio_bulk,
                                          ion_type, z_water_value=1), kw)
})
