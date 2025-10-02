test_that("gen_quadratic() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_quadratic(n = 500))
  testthat::expect_error(gen_quadratic(n = -500))

})

test_that("gen_cubic() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_cubic(n = 500))
  testthat::expect_error(gen_cubic(n = -500))

})
