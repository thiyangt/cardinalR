test_that("gen_swissroll() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_swissroll(n = 500))
  testthat::expect_error(gen_swissroll(n = -500))

})
