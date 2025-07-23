test_that("gen_crescent() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_crescent(n = 500, p = 4))
  testthat::expect_error(gen_crescent(n = -500, p = 4))
  testthat::expect_error(gen_crescent(n = 500, p = 1))
})


test_that("gen_curvycylinder() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_curvycylinder(n = 500, p = 4, h = 10))
  testthat::expect_error(gen_curvycylinder(n = -500, p = 4, h = 10))
  testthat::expect_error(gen_curvycylinder(n = 500, p = 3, h = 10))
  testthat::expect_error(gen_curvycylinder(n = 500, p = 4, h = -10))

})

test_that("gen_sphericalspiral() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_sphericalspiral(n = 500, p = 4, spins = 1))
  testthat::expect_error(gen_sphericalspiral(n = -500, p = 4, spins = 1))
  testthat::expect_error(gen_sphericalspiral(n = 500, p = 2, spins = 1))
  testthat::expect_error(gen_sphericalspiral(n = 500, p = 4, spins = -1))

})

test_that("gen_helicalspiral() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_helicalspiral(n = 500, p = 4))
  testthat::expect_error(gen_helicalspiral(n = -500, p = 4))
  testthat::expect_error(gen_helicalspiral(n = 500, p = 2))

})

test_that("gen_conicspiral() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_conicspiral(n = 500, p = 4, spins = 1))
  testthat::expect_error(gen_conicspiral(n = -500, p = 4, spins = 1))
  testthat::expect_error(gen_conicspiral(n = 500, p = 2, spins = 1))
  testthat::expect_error(gen_conicspiral(n = 500, p = 4, spins = -1))

})

test_that("gen_nonlinear() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_nonlinear(n = 500, p = 4, hc = 1, non_fac = 0.5))
  testthat::expect_error(gen_nonlinear(n = -500, p = 4, hc = 1, non_fac = 0.5))
  testthat::expect_error(gen_nonlinear(n = 500, p = 2, hc = 1, non_fac = 0.5))
  testthat::expect_error(gen_nonlinear(n = 500, p = 4, hc = -1, non_fac = 0.5))
  testthat::expect_error(gen_nonlinear(n = 500, p = 4, hc = 1, non_fac = -0.5))

})
