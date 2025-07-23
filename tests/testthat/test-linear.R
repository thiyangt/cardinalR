test_that("gen_longlinear() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_longlinear(n = 500, p = 4))
  testthat::expect_error(gen_longlinear(n = 500, p = 1))
  testthat::expect_error(gen_longlinear(n = -500, p = 4))

})
