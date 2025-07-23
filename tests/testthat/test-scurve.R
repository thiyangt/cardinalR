test_that("gen_scurve() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_scurve(n = 500, p = 4))
  testthat::expect_error(gen_scurve(n = -500, p = 4))
  testthat::expect_error(gen_scurve(n = 500, p = 2))

})

test_that("gen_scurvehole() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_scurvehole(n = 500, p = 4))
  testthat::expect_error(gen_scurvehole(n = -500, p = 4))
  testthat::expect_error(gen_scurvehole(n = 500, p = 2))

})
