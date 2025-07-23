test_that("gen_expbranches() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_expbranches(n = 400, p = 4, k = 4))
  testthat::expect_error(gen_expbranches(n = 400, p = 1, k = 4))
  testthat::expect_error(gen_expbranches(n = -400, p = 4, k = 4))
  testthat::expect_error(gen_expbranches(n = 400, p = 4, k = 0))

})

test_that("gen_orgcurvybranches() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_orgcurvybranches(n = 400, p = 4, k = 4))
  testthat::expect_error(gen_orgcurvybranches(n = 400, p = 1, k = 4))
  testthat::expect_error(gen_orgcurvybranches(n = -400, p = 4, k = 4))
  testthat::expect_error(gen_orgcurvybranches(n = 400, p = 4, k = 0))

})

test_that("gen_orglinearbranches() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_orglinearbranches(n = 400, p = 4, k = 4))
  testthat::expect_error(gen_orglinearbranches(n = 400, p = 1, k = 4))
  testthat::expect_error(gen_orglinearbranches(n = -400, p = 4, k = 4))
  testthat::expect_error(gen_orglinearbranches(n = 400, p = 4, k = 0))

})

test_that("gen_linearbranches() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_linearbranches(n = 400, p = 4, k = 4))
  testthat::expect_error(gen_linearbranches(n = 400, p = 1, k = 4))
  testthat::expect_error(gen_linearbranches(n = -400, p = 4, k = 4))
  testthat::expect_error(gen_linearbranches(n = 400, p = 4, k = 0))

})

test_that("gen_curvybranches() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_curvybranches(n = 400, p = 4, k = 4))
  testthat::expect_error(gen_curvybranches(n = 400, p = 1, k = 4))
  testthat::expect_error(gen_curvybranches(n = -400, p = 4, k = 4))
  testthat::expect_error(gen_curvybranches(n = 400, p = 4, k = 0))

})
