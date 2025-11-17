test_that("Test Lyso Pump equation", {

  #binary test
  expect_equal(calc_lyso_pump("B", 0, 0, 0), 1)
  expect_equal(calc_lyso_pump("B", 1, 0, 0), 1)
  expect_equal(calc_lyso_pump("B", 0, 1, 0), 0.55)
  expect_equal(calc_lyso_pump("B", 0, 0, 1), 1.818181818)
  expect_equal(calc_lyso_pump("B", 1, 1, 1), 1)
  expect_equal(calc_lyso_pump("A", 234, -23, 342), 1)

  #random example
  expect_equal(calc_lyso_pump("B", 13.1, 2.3, 4.5), 158.4893189)

  #from VBA
  #5
  lyso_pump <- 498.692860547534
  expect_equal(calc_lyso_pump("B", 10.1, 5.1, 7.8), lyso_pump)
})

test_that("Test Lyso membrane volume fraction equation", {
  expect_error(calc_lyso_membrane_vol_frac(0, 0))
  expect_equal(calc_lyso_membrane_vol_frac(1, 0), 0)
  expect_error(calc_lyso_membrane_vol_frac(0, 1))
  expect_error(calc_lyso_membrane_vol_frac(1, 1))

  #from VBA
  expect_equal(calc_lyso_membrane_vol_frac(520, 4), 2.27237141556669E-02)
})

test_that("Test Lysosome sequestration distribution ratio equation", {
  distribution_ratio_list <- list(DMW=0, DMWAN=0)
  expect_equal(calc_dlysw(distribution_ratio_list, 0, 0, 0), 0)
  expect_equal(calc_dlysw(distribution_ratio_list, 1, 0, 0), 0)
  expect_equal(calc_dlysw(distribution_ratio_list, 0, 1, 0), 1)
  expect_equal(calc_dlysw(distribution_ratio_list, 0, 0, 1), 0)

  distribution_ratio_list <- list(DMW=1, DMWAN=1)
  expect_equal(calc_dlysw(distribution_ratio_list, 1, 1, 1), 1)

  #from VBA
  #1
  distribution_ratio_list <- list(DMW=473151.25896148, DMWAN=473151.25896148)
  lyso_membrane_vol_frac <- 2.27237141556669E-02
  lyso_pump <- 1
  anionic_frac <- 0.175
  expect_equal(calc_dlysw(distribution_ratio_list, lyso_membrane_vol_frac, lyso_pump, anionic_frac), 10752.7312373204)

  #7
  distribution_ratio_list <- list(DMW=49438.719053445, DMWAN=943942.969970408)
  lyso_membrane_vol_frac <- 2.27237141556669E-02
  lyso_pump <- 498.692860547534
  anionic_frac <- 0.175
  expect_equal(calc_dlysw(distribution_ratio_list, lyso_membrane_vol_frac, lyso_pump, anionic_frac), 2334650.02846369)
})

test_that("Test Applying lysosome sequestration on fish-water distribution ratio", {
  expect_equal(apply_lyso_seq_dfw(0, 0, 0), 0)
  expect_equal(apply_lyso_seq_dfw(1, 0, 0), 1)
  expect_equal(apply_lyso_seq_dfw(0, 1, 0), 0)
  expect_equal(apply_lyso_seq_dfw(0, 0, 1), 0)
  expect_equal(apply_lyso_seq_dfw(1, 1, 1), 1)

  #from VBA
  #2
  dfw <- 152.63546355211
  dlysw <- 91.9621362490311
  lyso_to_body_frac <- 0.001568
  expect_equal(apply_lyso_seq_dfw(dfw, dlysw, lyso_to_body_frac), 152.540327774899)

  #6
  dfw <- 2630.05415844871
  dlysw <- 2334650.02846369
  lyso_to_body_frac <- 0.001568
  expect_equal(apply_lyso_seq_dfw(dfw, dlysw, lyso_to_body_frac), 6286.66147815932)
})

