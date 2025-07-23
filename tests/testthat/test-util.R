test_that("gen_noisedims() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_noisedims(n = 500, p = 4, m = c(0, 0, 0, 0), s = c(2, 2, 2, 2)))
  testthat::expect_error(gen_noisedims(n = -500, p = 4, m = c(0, 0, 0, 0), s = c(2, 2, 2, 2)))
  testthat::expect_error(gen_noisedims(n = 500, p = -4, m = c(0, 0, 0, 0), s = c(2, 2, 2, 2)))
  testthat::expect_error(gen_noisedims(n = 500, p = 4, m = c(0, 0, 0), s = c(2, 2, 2, 2)))
  testthat::expect_error(gen_noisedims(n = 500, p = 4, m = c(0, 0, 0, 0), s = c(2, 2, 2)))

})

test_that("gen_bkgnoise() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_bkgnoise(n = 500, p = 4, m = c(0, 0, 0, 0), s = c(2, 2, 2, 2)))
  testthat::expect_error(gen_bkgnoise(n = -500, p = 4, m = c(0, 0, 0, 0), s = c(2, 2, 2, 2)))
  testthat::expect_error(gen_bkgnoise(n = 500, p = -4, m = c(0, 0, 0, 0), s = c(2, 2, 2, 2)))
  testthat::expect_error(gen_bkgnoise(n = 500, p = 4, m = c(0, 0, 0), s = c(2, 2, 2, 2)))
  testthat::expect_error(gen_bkgnoise(n = 500, p = 4, m = c(0, 0, 0, 0), s = c(2, 2, 2)))
})

test_that("randomize_rows() works", {
  set.seed(20240412)
  testthat::expect_snapshot(randomize_rows(mobiusgau))
})

test_that("relocate_clusters() works", {
  set.seed(20240412)

  df <- tibble::tibble(
    x1 = rnorm(12),
    x2 = rnorm(12),
    x3 = rnorm(12),
    x4 = rnorm(12),
    cluster = rep(1:3, each = 4)
  )

  # Create a 3x4 matrix to define new cluster centers
  vert_mat <- matrix(c(
    5, 0, 0, 0,   # Shift cluster 1
    0, 5, 0, 0,   # Shift cluster 2
    0, 0, 5, 0    # Shift cluster 3
  ), nrow = 3, byrow = TRUE)

  testthat::expect_snapshot(relocate_clusters(data = df, vert_mat = vert_mat))
  testthat::expect_error(relocate_clusters(data = df |> dplyr::select(-cluster),
                                           vert_mat))
  testthat::expect_error(relocate_clusters(data = df, vert_mat = 3))
  testthat::expect_error(relocate_clusters(data = df, vert_mat = vert_mat[, 1]))
  testthat::expect_error(relocate_clusters(data = df, vert_mat = vert_mat[3, ]))

})

test_that("gen_nproduct() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_nproduct(7, 2))
  testthat::expect_error(gen_nproduct(-7, 2))
  testthat::expect_error(gen_nproduct(7, -2))
})

test_that("gen_nsum() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_nsum(30, 3))
  testthat::expect_error(gen_nsum(-30, 3))
  testthat::expect_error(gen_nsum(30, -3))
})

test_that("gen_wavydims1() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_wavydims1(n = 500, p = 4, theta = seq(pi / 6, 12 * pi / 6, length.out = 500)))
  testthat::expect_error(gen_wavydims1(n = -500, p = 4, theta = seq(pi / 6, 12 * pi / 6, length.out = 500)))
  testthat::expect_error(gen_wavydims1(n = 500, p = -4, theta = seq(pi / 6, 12 * pi / 6, length.out = 500)))
  testthat::expect_error(gen_wavydims1(n = 500, p = -4, theta = seq(pi / 6, 12 * pi / 6, length.out = 100)))

})

test_that("gen_wavydims2() works", {
  set.seed(20240412)
  theta <- seq(0, 2 * pi, length.out = 500)
  x1 <- sin(pi) * cos(theta)

  testthat::expect_snapshot(gen_wavydims2(n = 500, p = 4, x1_vec = x1))
  testthat::expect_error(gen_wavydims2(n = -500, p = 4, x1_vec = x1))
  testthat::expect_error(gen_wavydims2(n = 500, p = -4, x1_vec = x1))
  testthat::expect_error(gen_wavydims2(n = 500, p = 4, x1_vec = c(4, 2, 1)))

})

test_that("gen_wavydims3() works", {
  set.seed(20240412)
  df <- gen_scurve(n = 500, p = 4) |>
    as.matrix()

  testthat::expect_snapshot(gen_wavydims3(n = 500, p = 4, data = df))
  testthat::expect_error(gen_wavydims3(n = -500, p = 4, data = df))
  testthat::expect_error(gen_wavydims3(n = 500, p = -4, data = df))
  testthat::expect_error(gen_wavydims3(n = 500, p = 4, data = 9))

})

test_that("gen_rotation() works", {
  set.seed(20240412)

  rotations_4d <- list(
    list(plane = c(1, 2), angle = 60), # Rotation in the (1, 2) plane
    list(plane = c(3, 4), angle = 90)  # Rotation in the (3, 4) plane
  )

  testthat::expect_snapshot(gen_rotation(p = 4, planes_angles = rotations_4d))
  testthat::expect_error(gen_rotation(p = -4, planes_angles = rotations_4d))
  testthat::expect_error(gen_rotation(p = 4, planes_angles = 5))
})

test_that("normalize_data() works", {
  set.seed(20240412)

  gau_data <- gen_gaussian(n= 500, p = 4)
  testthat::expect_snapshot(normalize_data(data = gau_data))
})

test_that("gen_clustloc() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_clustloc(p = 4, k = 3))
  testthat::expect_error(gen_clustloc(p = -4, k = 3))
  testthat::expect_error(gen_clustloc(p = 4, k = -3))
})
