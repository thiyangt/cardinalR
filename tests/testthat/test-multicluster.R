test_that("gen_multicluster() works", {
  set.seed(20240412)

  rotations_4d <- list(
  cluster1 = list(
    list(plane = c(1, 2), angle = 60), # Rotation in the (1, 2) plane
    list(plane = c(3, 4), angle = 90)  # Rotation in the (3, 4) plane
    ),
  cluster2 = list(
    list(plane = c(1, 3), angle = 30) # Rotation in the (1, 3) plane
    ),
  cluster3 = list(
    list(plane = c(2, 4), angle = 45) # Rotation in the (2, 4) plane
    )
  )
  clust_data <- gen_multicluster(n = c(200, 300, 500), p = 4, k = 3,
                                loc = matrix(c(
                                  0, 0, 0, 0,
                                  5, 9, 0, 0,
                                  3, 4, 10, 7
                                ), nrow = 3, byrow = TRUE),
                                scale = c(3, 1, 2),
                                shape = c("gaussian", "cone", "unifcube"),
                                rotation = rotations_4d,
                                is_bkg = FALSE)

  testthat::expect_snapshot(clust_data)

  testthat::expect_error(gen_multicluster(n = c(200, 300, 500), p = 1, k = 3,
                                          loc = matrix(c(
                                            0, 0, 0, 0,
                                            5, 9, 0, 0,
                                            3, 4, 10, 7
                                          ), nrow = 3, byrow = TRUE),
                                          scale = c(3, 1, 2),
                                          shape = c("gaussian", "cone", "unifcube"),
                                          rotation = rotations_4d,
                                          is_bkg = FALSE))

  testthat::expect_error(gen_multicluster(n = c(200, 300, 500), p = 4, k = 0,
                                          loc = matrix(c(
                                            0, 0, 0, 0,
                                            5, 9, 0, 0,
                                            3, 4, 10, 7
                                          ), nrow = 3, byrow = TRUE),
                                          scale = c(3, 1, 2),
                                          shape = c("gaussian", "cone", "unifcube"),
                                          rotation = rotations_4d,
                                          is_bkg = FALSE))

  testthat::expect_error(gen_multicluster(n = c(200, -300, 500), p = 4, k = 3,
                                             loc = matrix(c(
                                               0, 0, 0, 0,
                                               5, 9, 0, 0,
                                               3, 4, 10, 7
                                             ), nrow = 3, byrow = TRUE),
                                             scale = c(3, 1, 2),
                                             shape = c("gaussian", "cone", "unifcube"),
                                             rotation = rotations_4d,
                                             is_bkg = FALSE))

  testthat::expect_error(gen_multicluster(n = c(200, 300, 500, 800), p = 4, k = 3,
                                             loc = matrix(c(
                                               0, 0, 0, 0,
                                               5, 9, 0, 0,
                                               3, 4, 10, 7
                                             ), nrow = 3, byrow = TRUE),
                                             scale = c(3, 1, 2),
                                             shape = c("gaussian", "cone", "unifcube"),
                                             rotation = rotations_4d,
                                             is_bkg = FALSE))

  testthat::expect_error(gen_multicluster(n = c(200, 300, 500), p = 4, k = 3,
                                             loc = matrix(c(
                                               0, 0, 0, 0,
                                               5, 9, 0, 0,
                                               3, 4, 10, 7
                                             ), nrow = 3, byrow = TRUE),
                                             scale = c(3, 1, 2, 6),
                                             shape = c("gaussian", "cone", "unifcube"),
                                             rotation = rotations_4d,
                                             is_bkg = FALSE))

  testthat::expect_error(gen_multicluster(n = c(200, 300, 500), p = 4, k = 3,
                                             loc = matrix(c(
                                               0, 0, 0, 0,
                                               5, 9, 0, 0,
                                               3, 4, 10, 7
                                             ), nrow = 3, byrow = TRUE),
                                             scale = c(3, 1, -2),
                                             shape = c("gaussian", "cone", "unifcube"),
                                             rotation = rotations_4d,
                                             is_bkg = FALSE))

  testthat::expect_error(gen_multicluster(n = c(200, 300, 500), p = 4, k = 3,
                                             loc = matrix(c(
                                               0, 0, 0, 0,
                                               5, 9, 0, 0,
                                               3, 4, 10, 7
                                             ), nrow = 3, byrow = TRUE),
                                             scale = c(3, 1, 2),
                                             shape = c("gaussian", "cone", "unifcube", "mobius"),
                                             rotation = rotations_4d,
                                             is_bkg = FALSE))

  testthat::expect_error(gen_multicluster(n = c(200, 300, 500), p = 4, k = 3,
                                             loc = 8,
                                             scale = c(3, 1, 2),
                                             shape = c("gaussian", "cone", "unifcube"),
                                             rotation = rotations_4d,
                                             is_bkg = FALSE))

  testthat::expect_error(gen_multicluster(n = c(200, 300, 500), p = 4, k = 3,
                                             loc = matrix(c(
                                               0, 0, 0, 0,
                                               5, 9, 0, 0
                                             ), nrow = 2, byrow = TRUE),
                                             scale = c(3, 1, 2),
                                             shape = c("gaussian", "cone", "unifcube"),
                                             rotation = rotations_4d,
                                             is_bkg = FALSE))

  testthat::expect_error(gen_multicluster(n = c(200, 300, 500), p = 4, k = 3,
                                             loc = matrix(c(
                                               0, 0, 0,
                                               5, 9, 0,
                                               3, 4, 10
                                             ), nrow = 3, byrow = TRUE),
                                             scale = c(3, 1, 2),
                                             shape = c("gaussian", "cone", "unifcube"),
                                             rotation = rotations_4d,
                                             is_bkg = FALSE))

})
