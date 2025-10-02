test_that("gen_mobius() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_mobius(n = 500))
  testthat::expect_error(gen_mobius(n = -500))

})
