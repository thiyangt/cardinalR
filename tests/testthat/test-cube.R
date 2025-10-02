test_that("gen_gridcube() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_gridcube(n = 500, p = 4))
  testthat::expect_snapshot(gen_gridcube(n = 500, p = 5))
  testthat::expect_error(gen_gridcube(n = -500, p = 4))
  testthat::expect_error(gen_gridcube(n = 500, p = -4))

})

test_that("gen_unifcube() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_unifcube(n = 500, p = 4))
  testthat::expect_snapshot(gen_unifcube(n = 500, p = 5))
  testthat::expect_error(gen_unifcube(n = -500, p = 4))
  testthat::expect_error(gen_unifcube(n = 500, p = -4))

})

test_that("gen_unifcubehole() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_unifcubehole(n = 1000, p = 4))
  testthat::expect_snapshot(gen_unifcubehole(n = 1000, p = 10))
  testthat::expect_error(gen_unifcubehole(n = -1000, p = 4))
  testthat::expect_error(gen_unifcubehole(n = 1000, p = -4))

})
