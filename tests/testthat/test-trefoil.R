test_that("gen_trefoil4d() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_trefoil4d(n = 500, p = 4, steps = 5))
  testthat::expect_error(gen_trefoil4d(n = -500, p = 4, steps = 5))
  testthat::expect_error(gen_trefoil4d(n = 500, p = 2, steps = 5))
  testthat::expect_error(gen_trefoil4d(n = 500, p = 4, steps = -5))

})

test_that("gen_trefoil3d() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_trefoil3d(n = 500, p = 4, steps = 5))
  testthat::expect_error(gen_trefoil3d(n = -500, p = 4, steps = 5))
  testthat::expect_error(gen_trefoil3d(n = 500, p = 2, steps = 5))
  testthat::expect_error(gen_trefoil3d(n = 500, p = 4, steps = -5))

})
