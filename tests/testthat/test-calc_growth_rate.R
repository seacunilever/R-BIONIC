test_that("Examples of using the function", {

  #binary check(using calculator)
  expect_error(calc_growth_rate(0,0))
  expect_equal(calc_growth_rate(1,0), 0.0001729793773)
  expect_error(calc_growth_rate(0,1))

  #using default check(using calculator)
  expect_equal(calc_growth_rate(0.5), 0.002887881954)

  #using data from Spreadsheet check
  expect_equal(calc_growth_rate(0.01, 12), 1.57013249362404E-03, tolerance = 1e-14)
})

test_that("Error from using different data types", {

  expect_error(calc_growth_rate("hello"))
  expect_error(calc_growth_rate(23, list("hello"="hello")))
})
