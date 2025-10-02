test_that("gen_cone() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_cone(n = 500, h = 5, ratio = 0.5))
  testthat::expect_error(gen_cone(n = 500, h = 5, ratio = -0.5))
  testthat::expect_error(gen_cone(n = -500, h = 5, ratio = 0.5))
  testthat::expect_error(gen_cone(n = 500, h = -5, ratio = 0.5))
  testthat::expect_error(gen_cone(n = 500, h = 5, ratio = 1.5))

})
