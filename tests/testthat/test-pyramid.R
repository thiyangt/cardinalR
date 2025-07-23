test_that("gen_pyrrect() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_pyrrect(n = 500, p = 4, h = 5, l_vec = c(3, 2), rt = 0.5))
  testthat::expect_error(gen_pyrrect(n = 500, p = 1, h = 5, l_vec = c(3, 2), rt = 0.5))
  testthat::expect_error(gen_pyrrect(n = -500, p = 4, h = 5, l_vec = c(3, 2), rt = 0.5))
  testthat::expect_error(gen_pyrrect(n = 500, p = 4, h = -5, l_vec = c(3, 2), rt = 0.5))
  testthat::expect_error(gen_pyrrect(n = 500, p = 4, h = 5, l_vec = c(-3, 2), rt = 0.5))
  testthat::expect_error(gen_pyrrect(n = 500, p = 4, h = 5, l_vec = c(3, 2), rt = -0.5))
  testthat::expect_error(gen_pyrrect(n = 500, p = 4, h = 5, l_vec = c(3, 2), rt = 4.5))

})

test_that("gen_pyrtri() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_pyrtri(n = 500, p = 4, h = 5, l = 3, rt = 0.5))
  testthat::expect_error(gen_pyrtri(n = 500, p = 1, h = 5, l = 3, rt = 0.5))
  testthat::expect_error(gen_pyrtri(n = -500, p = 4, h = 5, l = 3, rt = 0.5))
  testthat::expect_error(gen_pyrtri(n = 500, p = 4, h = -5, l = 3, rt = 0.5))
  testthat::expect_error(gen_pyrtri(n = 500, p = 4, h = 5, l = -3, rt = 0.5))
  testthat::expect_error(gen_pyrtri(n = 500, p = 4, h = 5, l = 3, rt = -0.5))
  testthat::expect_error(gen_pyrtri(n = 500, p = 4, h = 5, l = 3, rt = 5.5))

})

test_that("gen_pyrstar() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_pyrstar(n = 500, p = 4, h = 5, rb = 3))
  testthat::expect_error(gen_pyrstar(n = 500, p = 1, h = 5, rb = 3))
  testthat::expect_error(gen_pyrstar(n = -500, p = 4, h = 5, rb = 3))
  testthat::expect_error(gen_pyrstar(n = 500, p = 4, h = -5, rb = 3))
  testthat::expect_error(gen_pyrstar(n = 500, p = 4, h = 5, rb = -3))

})

test_that("gen_pyrholes() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_pyrholes(n = 500, p = 3))
  testthat::expect_error(gen_pyrholes(n = 500, p = 1))
  testthat::expect_error(gen_pyrholes(n = c(500, 200), p = 3))
  testthat::expect_error(gen_pyrholes(n = -500, p = 3))

})
