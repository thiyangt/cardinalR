#' Generate Small Spheres Within a Big Sphere
#'
#' This function generates a dataset representing a structure with a small and big spheres.
#'
#' @param n A numeric vector (default: c(1000, 100)) representing the sample sizes of the big and small spheres respectively.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param k A numeric value (default: 3) representing the number of small spheres.
#' @param r A numeric vector (default: c(15, 3)) representing the radius of the big and small spheres respectively.
#' @param loc A numeric value (default: 10 / sqrt(3) representing how far the small spheres are placed from each other.
#' @return A data containing small spheres within a big sphere.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' sphere_data <- gen_clusteredSpheres(n = 500, p = 4, r = 1)
#' head(sphere_data, 5)
gen_clusteredSpheres <- function(k = 3, p = 4, n = c(1000, 100), r = c(15, 3),
                                 loc = 10 / sqrt(3)) {

  if (p < 3) {
    cli::cli_abort("p should be greater than 3.")
  }

  if (length(n) != 2) {
    cli::cli_abort("n should contain exactly two values.")
  }

  if (any(n < 0)) {
    cli::cli_abort("Values in n should be positive.")
  }

  if (any(r < 0)) {
    cli::cli_abort("Values in r should be positive.")
  }

  n_big <- n[1]
  n_small <- n[2]

  r_big <- r[1]
  r_small <- r[2]

  d_dim_sphere <- gen_unifSphere(n_small, p, r_small)
  small_spheres <- lapply(seq_len(k), function(i) {
    center <- stats::rnorm(p, sd = loc)
    sweep(d_dim_sphere, 2, center, "+")
  })

  big_sphere <- gen_unifSphere(n_big, p, r_big)

  small_labeled <- lapply(seq_along(small_spheres), function(i) {
    cbind(small_spheres[[i]], cluster = paste0("small_", i))
  })

  big_labeled <- cbind(big_sphere, cluster = "big")

  df <- dplyr::bind_rows(c(small_labeled, list(big_labeled))) |>
    tibble::as_tibble()
  names(df) <- append(paste0("x", 1:p), "cluster")

  ## Swap rows
  df <- randomize_rows(df)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}

#' Generate p-D Triangular Pyramid With Triangular Pyramid shaped holes
#'
#' This function generates p-D triangular pyramid with triangular pyramid shaped holes.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing a triangular pyramid with triangular pyramid shaped holes.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' triangular_data <- gen_pyrHoles(n = 500, p = 3)
gen_pyrHoles <- function(n = 500, p = 4) {

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (length(n) != 1) {
    cli::cli_abort("n should be a single integer specifying the number of points.")
  }

  if (n < 0) {
    cli::cli_abort("n should be positive.")
  }

  trace_point <- stats::runif(p)
  corner_points <- geozoo::simplex(p=p)$points

  data_list <- lapply(1:p, function(i) rep(0, n))
  names(data_list) <- paste0("x", 1:p)

  df <- tibble::tibble(!!!data_list)

  for (i in 1:n) {
    trace_point <- (corner_points[sample((p + 1), 1), ] + trace_point) / 2
    df[i, ] <- as.list(trace_point)
  }

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}

#' Generate Gaussian cluster with the Mobius Cluster
#'
#' This function generates a dataset consisting of a mobius cluster and Gaussian cluster.
#'
#' @param n A numeric vector (default: c(200, 100)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing the mobius cluster and Gaussian cluster.
#' @export
#'
#' @examples
#' data <- gen_mobiusGau(n = c(200, 100), p = 4)
gen_mobiusGau <- function(n = c(200, 100), p = 4) {

  if (p < 3) {
    cli::cli_abort("p should be greater than 3.")
  }

  if (length(n) != 2) {
    cli::cli_abort("n should contain exactly two values.")
  }

  if (any(n < 0)) {
    cli::cli_abort("Values in n should be positive.")
  }

  df1 <- gen_mobius(n = n[1], p = p) |>
    dplyr::mutate(cluster = "cluster1")

  ## To add background noise
  df2 <- gen_gaussian(n = n[2], p = p, m = rep(0, p), s = diag(p) * 0.01) |>
    dplyr::mutate(cluster = "cluster2")

  df <- dplyr::bind_rows(df1, df2)

  ## Swap rows
  df <- randomize_rows(df)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}


#' Generate overlapped two conic spiral shapes
#'
#' This function generates overlapped tow conic spiral shapes.
#'
#' @param n A numeric vector (default: c(500, 500)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#'
#' @return A data containing a two overlapped conic spiral shapes.
#'
#' @examples
#' set.seed(20240412)
#' data <- gen_overlapConicSpirals(n = c(500, 500), p = 4)
#'
#' @export
gen_overlapConicSpirals <- function(n = c(500, 500), p = 4) {

  if (p < 3) {
    cli::cli_abort("p should be greater than 3.")
  }

  if (length(n) != 2) {
    cli::cli_abort("n should contain exactly 2 values.")
  }

  if (any(n < 0)) {
    cli::cli_abort("Values in n should be positive.")
  }

  df1 <- tibble::as_tibble(geozoo::conic.spiral(n = n[1])$points, .name_repair = "minimal") |>
    rlang::set_names(paste0("x", 1:3)) |>
    dplyr::mutate(cluster = "cluster1")

  df2 <- tibble::as_tibble(geozoo::conic.spiral(n = n[2])$points[,c(3, 1, 2)], .name_repair = "minimal") |>
    rlang::set_names(paste0("x", 1:3)) |>
    dplyr::mutate(cluster = "cluster2")

  df <- dplyr::bind_rows(df1, df2)

  if (p > 3) {

    noise_df <- gen_noisedims(n = NROW(df), p = (p-3), m = rep(0, p-3), s = rep(0.05, p-3))
    colnames(noise_df) <- paste0("x", 4:p)

    df <- dplyr::bind_cols(df, noise_df) |>
      dplyr::select(dplyr::starts_with("x"), "cluster")

  }

  ## Swap rows
  df <- randomize_rows(df)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)

}

#' Generate intersected two conic spiral shapes
#'
#' This function generates intersected tow conic spiral shapes.
#'
#' @param n A numeric vector (default: c(500, 500)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#'
#' @return A data containing the two intersected conic spiral shapes.
#'
#' @examples
#' set.seed(20240412)
#' data <- gen_intersectConicSpirals(n = c(500, 500), p = 4)
#'
#' @export
gen_intersectConicSpirals <- function(n = c(500, 500), p = 4) {

  if (p < 3) {
    cli::cli_abort("p should be greater than 3.")
  }

  if (length(n) != 2) {
    cli::cli_abort("n should contain exactly 2 values.")
  }

  if (any(n < 0)) {
    cli::cli_abort("Values in n should be positive.")
  }

  df1 <- tibble::as_tibble(geozoo::conic.spiral(n = n[1])$points, .name_repair = "minimal") |>
    rlang::set_names(paste0("x", 1:3)) |>
    dplyr::mutate(cluster = "cluster1")

  df2 <- tibble::as_tibble(geozoo::conic.spiral(n = n[2])$points[,c(2, 1, 3)], .name_repair = "minimal") |>
    rlang::set_names(paste0("x", 1:3)) |>
    dplyr::mutate(cluster = "cluster2")

  df <- dplyr::bind_rows(df1, df2)

  if (p > 3) {

    noise_df <- gen_noisedims(n = NROW(df), p = (p-3), m = rep(0, p-3), s = rep(0.05, p-3))
    colnames(noise_df) <- paste0("x", 4:p)

    df <- dplyr::bind_cols(df, noise_df) |>
      dplyr::select(dplyr::starts_with("x"), "cluster")

  }

  ## Swap rows
  df <- randomize_rows(df)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)

}

#' Generate intersected two dini surface shapes
#'
#' This function generates intersected tow dini surface shapes.
#'
#' @param n A numeric vector (default: c(500, 500)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#'
#' @return A data containing the two intersected dini surface shapes.
#'
#' @examples
#' set.seed(20240412)
#' data <- gen_overlapDiniSurface(n = c(500, 500), p = 4)
#' @export
gen_overlapDiniSurface <- function(n = c(500, 500), p = 4) {

  if (p < 3) {
    cli::cli_abort("p should be greater than 3.")
  }

  if (length(n) != 2) {
    cli::cli_abort("n should contain exactly 2 values.")
  }

  if (any(n < 0)) {
    cli::cli_abort("Values in n should be positive.")
  }

  df1 <- tibble::as_tibble(geozoo::dini.surface(n = n[1])$points, .name_repair = "minimal") |>
    rlang::set_names(paste0("x", 1:3)) |>
    dplyr::mutate(cluster = "cluster1")

  df2 <- tibble::as_tibble(geozoo::dini.surface(n = n[2])$points[,c(2, 1, 3)], .name_repair = "minimal") |>
    rlang::set_names(paste0("x", 1:3)) |>
    dplyr::mutate(cluster = "cluster2")

  df <- dplyr::bind_rows(df1, df2)

  if (p > 3) {

    noise_df <- gen_noisedims(n = NROW(df), p = (p-3), m = rep(0, p-3), s = rep(0.05, p-3))
    colnames(noise_df) <- paste0("x", 4:p)

    df <- dplyr::bind_cols(df, noise_df) |>
      dplyr::select(dplyr::starts_with("x"), "cluster")

  }

  ## Swap rows
  df <- randomize_rows(df)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)

}

#' Generate overlapped two dini surface shapes
#'
#' This function generates overlapped tow dini surface shapes.
#'
#' @param n A numeric vector (default: c(500, 500)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#'
#' @return A data containing the two overlapped dini surface shapes.
#'
#' @examples
#' set.seed(20240412)
#' data <- gen_intersectDiniSurface(n = c(500, 500), p = 4)
#' @export
gen_intersectDiniSurface <- function(n = c(500, 500), p = 4) {

  if (p < 3) {
    cli::cli_abort("p should be greater than 3.")
  }

  if (length(n) != 2) {
    cli::cli_abort("n should contain exactly 2 values.")
  }

  if (any(n < 0)) {
    cli::cli_abort("Values in n should be positive.")
  }

  df1 <- tibble::as_tibble(geozoo::dini.surface(n = n[1])$points, .name_repair = "minimal") |>
    rlang::set_names(paste0("x", 1:3)) |>
    dplyr::mutate(cluster = "cluster1")

  df2 <- tibble::as_tibble(geozoo::dini.surface(n = n[2])$points[,c(3, 2, 1)], .name_repair = "minimal") |>
    rlang::set_names(paste0("x", 1:3)) |>
    dplyr::mutate(cluster = "cluster2")

  df <- dplyr::bind_rows(df1, df2)

  if (p > 3) {

    noise_df <- gen_noisedims(n = NROW(df), p = (p-3), m = rep(0, p-3), s = rep(0.05, p-3))
    colnames(noise_df) <- paste0("x", 4:p)

    df <- dplyr::bind_cols(df, noise_df) |>
      dplyr::select(dplyr::starts_with("x"), "cluster")

  }

  ## Swap rows
  df <- randomize_rows(df)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)

}

#' Generate two roman surface shaped clusters
#'
#' This function generates tow roman surface shaped clusters
#'
#' @param n A numeric vector (default: c(500, 500)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#'
#' @return A data containing the tow roman surface shaped clusters.
#'
#' @examples
#' set.seed(20240412)
#' data <- gen_clustRomanSurface(n = c(500, 500), p = 4)
#' @export
gen_clustRomanSurface <- function(n = c(500, 500), p = 4) {

  if (p < 3) {
    cli::cli_abort("p should be greater than 3.")
  }

  if (length(n) != 2) {
    cli::cli_abort("n should contain exactly 2 values.")
  }

  if (any(n < 0)) {
    cli::cli_abort("Values in n should be positive.")
  }

  df1 <- tibble::as_tibble(geozoo::roman.surface(n = n[1])$points + 1, .name_repair = "minimal") |>
    rlang::set_names(paste0("x", 1:3)) |>
    dplyr::mutate(cluster = "cluster1")

  df2 <- tibble::as_tibble(geozoo::roman.surface(n = n[2])$points[,c(3, 1, 2)], .name_repair = "minimal") |>
    rlang::set_names(paste0("x", 1:3)) |>
    dplyr::mutate(cluster = "cluster2")

  df <- dplyr::bind_rows(df1, df2)

  if (p > 3) {

    noise_df <- gen_noisedims(n = NROW(df), p = (p-3), m = rep(0, p-3), s = rep(0.05, p-3))
    colnames(noise_df) <- paste0("x", 4:p)

    df <- dplyr::bind_cols(df, noise_df) |>
      dplyr::select(dplyr::starts_with("x"), "cluster")

  }

  ## Swap rows
  df <- randomize_rows(df)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)

}

#' Generate overlapped two torus shapes
#'
#' This function generates overlapped tow torus shapes.
#'
#' @param n A numeric vector (default: c(500, 500)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#'
#' @return A data containing the two overlapped torus shapes.
#'
#' @examples
#' set.seed(20240412)
#' data <- gen_overlapTorus(n = c(500, 500), p = 4)
#'
#' @export
gen_overlapTorus <- function(n = c(500, 500), p = 4) {

  if (p < 3) {
    cli::cli_abort("p should be greater than 3.")
  }

  if (length(n) != 2) {
    cli::cli_abort("n should contain exactly 2 values.")
  }

  if (any(n < 0)) {
    cli::cli_abort("Values in n should be positive.")
  }

  df1 <- tibble::as_tibble(geozoo::torus(p = 3, n = n[1])$points, .name_repair = "minimal") |>
    rlang::set_names(paste0("x", 1:3)) |>
    dplyr::mutate(cluster = "cluster1")

  df2 <- tibble::as_tibble(geozoo::torus(p = 3, n = n[2])$points[,c(3, 1, 2)], .name_repair = "minimal") |>
    rlang::set_names(paste0("x", 1:3)) |>
    dplyr::mutate(cluster = "cluster2")

  df <- dplyr::bind_rows(df1, df2)

  if (p > 3) {

    noise_df <- gen_noisedims(n = NROW(df), p = (p-3), m = rep(0, p-3), s = rep(0.05, p-3))
    colnames(noise_df) <- paste0("x", 4:p)

    df <- dplyr::bind_cols(df, noise_df) |>
      dplyr::select(dplyr::starts_with("x"), "cluster")

  }

  ## Swap rows
  df <- randomize_rows(df)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)

}

#' Generate Curvy Branching Clusters with Noise
#'
#' This function generates data with curvy branching clusters along with added noise.
#'
#' @param n A numeric vector (default: c(300, 200)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate curvy branching clusters with noise with custom parameters
#' set.seed(20240412)
#' data <- gen_twoCurvy(n = c(300, 200), p = 4)
gen_twoCurvy <- function(n = c(300, 200), p = 4) {

  if (p < 4) {
    cli::cli_abort("p should be 4 or greater.")
  }

  if (length(n) != 2) {
    cli::cli_abort("n should contain exactly 2 values.")
  }

  if (any(n < 0)) {
    cli::cli_abort("Values in n should be positive.")
  }


  theta1 <- stats::runif(n[1], 0.20, 0.90 * pi)

  df1 <- tibble::tibble(
    x1 = cos(theta1) + stats::rnorm(n[1], 1, 0.06),
    x2 = sin(theta1) + stats::rnorm(n[1], 1, 0.06),
    x3 = cos(theta1) + stats::rnorm(n[1], 1, 0.06),
    x4 = sin(theta1) + stats::rnorm(n[1], 1, 0.06),
    cluster = "cluster1"
  )

  theta2 <- stats::runif(n[2], 0.20, 0.90 * pi)

  df2 <- tibble::tibble(
    x1 = cos(-theta2) + stats::rnorm(n[2], 1, 0.06),
    x2 = sin(-theta2) + stats::rnorm(n[2], 1, 0.06),
    x3 = cos(-theta2) + stats::rnorm(n[2], 1, 0.06),
    x4 = sin(-theta2) + stats::rnorm(n[2], 1, 0.06),
    cluster = "cluster2"
  )

  df <- dplyr::bind_rows(df1, df2)

  if (p > 5) {

    noise_df <- gen_noisedims(n = NROW(df), p = (p-4), m = rep(0, p-4), s = rep(0.05, p-4))
    colnames(noise_df) <- paste0("x", 5:p)

    df <- dplyr::bind_cols(df, noise_df) |>
      dplyr::select(dplyr::starts_with("x"), "cluster")

  }

  ## Swap rows
  df <- randomize_rows(df)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)

}


#' Generate Curvy Branching Cluster Data
#'
#' This function generates two curvy clusters and one Gaussian cluster in the middle.
#'
#' @param n A numeric vector (default: c(200, 200, 100)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing two curvy clusters and one Gaussian cluster.
#' @export
#'
#' @examples
#'
#' # Generate curvy branching cluster data with custom parameters
#' set.seed(20240412)
#' data <- gen_twoCurvyGau(n = c(200, 200, 100), p = 4)
gen_twoCurvyGau <- function(n = c(200, 200, 100), p = 4) {

  if (p < 4) {
    cli::cli_abort("p should be 4 or greater.")
  }

  if (length(n) != 3) {
    cli::cli_abort("n should contain exactly 3 values.")
  }

  if (any(n < 0)) {
    cli::cli_abort("Values in n should be positive.")
  }

  df1 <- gen_twoCurvy(n = n[1:2], p = p)
  df2 <- gen_gaussian(n = n[3], p = p, m = rep(1, p), s = diag(4) * 0.01) |>
    dplyr::mutate(cluster = "cluster3")

  df <- dplyr::bind_rows(df1, df2)

  ## Swap rows
  df <- randomize_rows(df)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)

}

#' Generate Curvy Branching Cluster Data with Background Noise
#'
#' This function generates data with two curvy clusters and one Gaussian cluster with background noise.
#'
#' @param n A numeric vector (default: c(200, 200, 100, 50)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing two curvy clusters and one Gaussian cluster with background noise.
#' @export
#'
#' @examples
#' # Generate curvy branching cluster data with background noise with custom parameters
#' set.seed(20240412)
#' data <- gen_twoCurvyGauBkg(n = c(200, 200, 100, 50), p = 4)
gen_twoCurvyGauBkg <- function(n = c(200, 200, 100, 50), p = 4) {

  if (p < 4) {
    cli::cli_abort("p should be 4 or greater.")

  }

  if (length(n) != 4) {
    cli::cli_abort("n should contain exactly 4 values.")
  }

  if (any(n < 0)) {
    cli::cli_abort("Values in n should be positive.")
  }


  df1 <- gen_twoCurvyGau(n = n[1:3], p = p)
  df2 <- gen_bkgnoise(n = n[4], p = p, m = rep(1, p), s = rep(1, p)) |>
    dplyr::mutate(cluster = "bkg_noise")

  df <- dplyr::bind_rows(df1, df2)

  ## Swap rows
  df <- randomize_rows(df)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)

}

#' Generate One Grid with Background Noise
#'
#' This function generates a grid data and background noise.
#'
#' @param n A numeric vector (default: c(10, 10)) representing the number of grid points along each axes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing one grid data with background noise.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' one_grid_bkg <- gen_oneGridBkg(n = c(10, 10), p = 4)
gen_oneGridBkg <- function(n = c(10, 10), p = 4) {

  if (length(n) != 2) {
    cli::cli_abort("n should contain exactly 2 values.")
  }

  if (any(n < 0)) {
    cli::cli_abort("Values in n should be positive.")
  }

  df1 <- gen_gridcube(n = n, p = 2)
  noise_df <- gen_noisedims(n = n[1] * n[2], p = (p-2), m = rep(0, p-2), s = rep(0.05, p-2))
  names(noise_df) <- paste0("x", 3:p)

  df1 <- dplyr::bind_cols(df1, noise_df)

  df2 <- gen_bkgnoise(n = NROW(df1) * 0.3, p = p, m = c(0, 0, 0, 0), s = c(2, 2, 2, 2))
  df <- dplyr::bind_rows(df1, df2)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}

#' Generate Two Grids with Different Offset
#'
#' This function generates two grids with an offset.
#'
#' @param n A numeric vector (default: c(10, 10)) representing the number of grid points along each axes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing two grids data.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' two_grid_comb <- gen_twoGrids(n = c(10, 10), p = 4)
gen_twoGrids <- function(n = c(10, 10), p = 4) {

  if (length(n) != 2) {
    cli::cli_abort("n should contain exactly 2 values.")
  }

  if (any(n < 0)) {
    cli::cli_abort("Values in n should be positive.")
  }

  df1 <- gen_gridcube(n = n, p = 2)

  df2 <- df1 + 3

  noise_df <- gen_noisedims(n = n[1] * n[2], p = (p-2), m = rep(0, p-2), s = rep(0.05, p-2))
  names(noise_df) <- paste0("x", 3:p)

  df1 <- dplyr::bind_cols(df1, noise_df)

  noise_df <- gen_noisedims(n = n[1] * n[2], p = (p-2), m = rep(0, p-2), s = rep(0.05, p-2))
  names(noise_df) <- paste0("x", 3:p)

  df2 <- dplyr::bind_cols(df2, noise_df)
  df <- dplyr::bind_rows(df1, df2)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}


#' Generate Two Grids with Background Noise
#'
#' This function generates two grids with background noise.
#'
#' @param n A numeric vector (default: c(10, 10)) representing the number of grid points along each axes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing two grids data with background noise.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' two_grid_comb_bkg <- gen_twoGridsBkg(n = c(10, 10), p = 4)
gen_twoGridsBkg <- function(n = c(10, 10), p = 4) {

  if (length(n) != 2) {
    cli::cli_abort("n should contain exactly 2 values.")
  }

  if (any(n < 0)) {
    cli::cli_abort("Values in n should be positive.")
  }

  df1 <- gen_gridcube(n = n, p = 2)
  df3 <- df1 + 5

  noise_df <- gen_noisedims(n = n[1] * n[2], p = (p-2), m = rep(0, p-2), s = rep(0.05, p-2))
  names(noise_df) <- paste0("x", 3:p)

  df1 <- dplyr::bind_cols(df1, noise_df)

  noise_df <- gen_noisedims(n = n[1] * n[2], p = (p-2), m = rep(0, p-2), s = rep(0.05, p-2))
  names(noise_df) <- paste0("x", 3:p)

  df3 <- dplyr::bind_cols(df3, noise_df)

  df1 <- dplyr::bind_rows(df1, df3)

  df2 <- gen_bkgnoise(n = NROW(df1) * 0.1, p = p, m = c(0, 0, 0, 0), s = c(2, 2, 2, 2))
  df <- dplyr::bind_rows(df1, df2)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}


#' Generate Long Cluster Data
#'
#' This function generates a dataset consisting of any number of long clusters.
#'
#' @param n A numeric vector (default: c(200, 500, 300)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param k A numeric value (default: 3) representing the number of clusters.
#' @return A data containing the long cluster data.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' long_cluster <- gen_longClust(n = c(200, 500, 300), p = 4, k = 3)
gen_longClust <- function(n = c(200, 500, 300), p = 4, k = 3) {

  if (k < 2) {
    cli::cli_abort("k should be greater than 2.")
  }

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (length(n) != k) {
    cli::cli_abort("n should contain exactly {.val {k}} values.")
  }

  if (any(n < 0)) {
    cli::cli_abort("Values in n should be positive.")
  }

  df <- tibble::tibble()

  for (j in 1:k) {

    df1 <- gen_longLinear(n = n[j], p = 4) |>
      dplyr::mutate(cluster = paste0("cluster", j))

    df <- dplyr::bind_rows(df, df1)

  }

  ## To swap rows
  df <- randomize_rows(df)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)

}

#' Generate Overlapped Any number of Curvy Circle Clusters
#'
#' This function generates a dataset representing a structure with any number of overlapped curvy circles.
#'
#' @param n A numeric vector (default: c(200, 500, 300)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param k A numeric value (default: 3) representing the number of clusters.
#' @return A data containing the any number of overlapped curvy circle clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' curvy_cell_cycle_data <- gen_overlappedCurvyCycle(n = c(200, 500, 300), p = 4, k = 3)
gen_overlappedCurvyCycle <- function(n = c(200, 500, 300), p = 4, k = 3) {

  if (k < 2) {
    cli::cli_abort("k should be greater than 2.")
  }

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (length(n) != k) {
    cli::cli_abort("n should contain exactly {.val {k}} values.")
  }

  if (any(n < 0)) {
    cli::cli_abort("Values in n should be positive.")
  }

  df <- tibble::tibble()

  for (i in 1:k) {

    shift_vec <- append(sample(seq(-0.5, 0.5, 0.2), 3), rep(0, p-3))
    scale_vec <- append(sample(seq(-0.5, 0.5, 0.2), 3), rep(1, p-3))

    df3 <- gen_curvycycle(n[i], p = p) |>
      purrr::map2(scale_vec, ~ .x * .y) |>
      dplyr::bind_cols() |>
      purrr::map2(shift_vec, ~ .x * .y) |>
      dplyr::bind_cols()

    df3 <- df3|>
      stats::setNames(names(df3)) |>
      tibble::as_tibble() |>
      dplyr::mutate(cluster = paste0("cluster", i))

    df <- dplyr::bind_rows(df, df3)

  }

  ## To swap rows
  df <- randomize_rows(df)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)

}

#' Generate Shited Any number of 2-D Circle Clusters
#'
#' This function generates a dataset representing a structure with any number of shifted circles.
#'
#' @param n A numeric vector (default: c(200, 500, 300)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param k A numeric value (default: 3) representing the number of clusters.
#' @return A data containing the any number of shifted circle clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' circular_clusters_data <- gen_shiftedCircleClusts(n = c(200, 500, 300), p = 4, k = 3)
gen_shiftedCircleClusts <- function(n = c(200, 500, 300), p = 4, k = 3) {

  if (k < 2) {
    cli::cli_abort("k should be greater than 2.")
  }

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (length(n) != k) {
    cli::cli_abort("n should contain exactly {.val {k}} values.")
  }

  if (any(n < 0)) {
    cli::cli_abort("Values in n should be positive.")
  }

  df <- tibble::tibble()

  for (i in 1:k) {
    ## Generate scale factors for circles
    scale_factors_vec <- stats::runif(1, 0, 2)

    df3 <- gen_circle(n[i], p = p) |>
      dplyr::mutate(across(where(is.numeric), ~ .x * scale_factors_vec)) |>
      dplyr::mutate(cluster = paste0("cluster", i))

    df <- dplyr::bind_rows(df, df3)

  }

  ## To swap rows
  df <- randomize_rows(df)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}

#' Generate Overlapped Any number of Circle Clusters
#'
#' This function generates a dataset representing a structure with any number of overlapped circles.
#'
#' @param n A numeric vector (default: c(200, 500, 300)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param k A numeric value (default: 3) representing the number of clusters.
#' @return A data containing the any number of overlapped circle clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' cell_cycle_data <- gen_overlappedCircleClusts(n = c(200, 500, 300), p = 4, k = 3)
gen_overlappedCircleClusts <- function(n = c(200, 500, 300), p = 4, k = 3) {

  if (k < 2) {
    cli::cli_abort("k should be greater than 2.")
  }

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (length(n) != k) {
    cli::cli_abort("n should contain exactly {.val {k}} values.")
  }

  if (any(n < 0)) {
    cli::cli_abort("Values in n should be positive.")
  }

  r1 <- 2
  r2 <- 1

  perms <- gtools::permutations(n = p, r = p)
  num_perms <- NROW(perms)

  # Ensure we don't try to sample more permutations than available
  selected_permute <- sample(1:num_perms, k, replace = FALSE)

  # Generate k datasets with swapped columns and varying 'n'
  swapped_datasets_varying_n <- lapply(1:k, function(i) {

    scale_factors <- append(c(2, 1), rep(1, p-2))

    generated_tibble <- gen_circle(n[i], p = p) |>
      purrr::map2(scale_factors, ~ .x * .y) |>
      dplyr::bind_cols()

    generated_tibble <- generated_tibble|>
      stats::setNames(names(generated_tibble)) |>
      tibble::as_tibble()

    # Permute the columns of the tibble
    perm_indices <- perms[selected_permute[i], ]
    df <- generated_tibble[, perm_indices]
    df <- df |>
      dplyr::mutate(cluster = paste0("cluster", i))
    names(df) <- append(paste0("x", 1:p), "cluster") # Ensure consistent column names

    df

  })

  df <- dplyr::bind_rows(swapped_datasets_varying_n)

  ## To swap rows
  df <- randomize_rows(df)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}

#' Generate Long Cluster Data
#'
#' This function generates a dataset consisting of two long clusters.
#'
#' @param n A numeric vector (default: c(200, 300)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing the long cluster data.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' long_cluster <- gen_twoLongClusts(n = c(200, 300), p = 4)
gen_twoLongClusts <- function(n = c(200, 300), p = 4) {

  if (p < 4) {
    stop(cli::cli_alert_danger("p should be 4 or greater."))
  }

  if (length(n) != 2) {
    stop(cli::cli_alert_danger("n should contain exactly 2 values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  x1 <- 0:(n[1] - 1) + 0.03 * n[1] * stats::rnorm(n[1])
  x2 <- 0:(n[1] - 1) + 0.03 * n[1] * stats::rnorm(n[1])
  x3 <- 0:(n[1] - 1) + 0.03 * n[1] * stats::rnorm(n[1])
  x4 <- 0:(n[1] - 1) + 0.03 * n[1] * stats::rnorm(n[1])
  df1 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- 0:(n[2] - 1) + 0.03 * n[2] * stats::rnorm(n[2]) + n[2] / 5
  x2 <- 0:(n[2] - 1) + 0.03 * n[2] * stats::rnorm(n[2]) - n[2] / 5
  x3 <- 0:(n[2] - 1) + 0.03 * n[2] * stats::rnorm(n[2]) + n[2] / 5
  x4 <- 0:(n[2] - 1) + 0.03 * n[2] * stats::rnorm(n[2]) - n[2] / 5
  df2 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  df <- dplyr::bind_rows(df1, df2)

  if (p > 4) {

    cli::cli_alert_info("Adding noise dimensions to reach the desired dimensionality.")

    noise_mat <- gen_noise_dims(
      n = NROW(df), num_noise = p - 4,
      min_n = -0.5, max_n = 0.5
    )
    colnames(noise_mat) <- paste0("x", 5:p)
    df <- dplyr::bind_cols(df, noise_mat)

  }

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}

#' Generate Three Different Linear Data
#'
#' This function generates a dataset consisting of three different linear clusters.
#'
#' @param n A numeric vector (default: c(200, 300, 150)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing the three different linear data.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' three_diff_linear <- gen_threeAngledLongClusts(n = c(200, 300, 150), p = 4)
gen_threeAngledLongClusts <- function(n = c(200, 300, 150), p = 4) {

  if (p < 4) {
    stop(cli::cli_alert_danger("p should be 4 or greater."))
  }

  if (length(n) != 3) {
    stop(cli::cli_alert_danger("n should contain exactly 3 values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  x1 <- 0:(n[1] - 1) + 0.03 * n[1] * stats::rnorm(n[1]) - 150
  x2 <- 0:(n[1] - 1) + 0.03 * n[1] * stats::rnorm(n[1]) - 20
  x3 <- 0:(n[1] - 1) + 0.03 * n[1] * stats::rnorm(n[1])
  x4 <- 0:(n[1] - 1) + 0.03 * n[1] * stats::rnorm(n[1]) - 65

  df1 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- 0:(n[2] - 1) + 0.03 * n[2] * stats::rnorm(n[2]) + n[2] / 5
  x2 <- 0:(n[2] - 1) + 0.03 * n[2] * stats::rnorm(n[2]) - n[2] / 5
  x3 <- 0:(n[2] - 1) + 0.03 * n[2] * stats::rnorm(n[2]) + n[2] / 5
  x4 <- 0:(n[2] - 1) + 0.03 * n[2] * stats::rnorm(n[2]) - n[2] / 5

  df2 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- -(0:(n[3] - 1) + 0.03 * n[3] * stats::rnorm(n[3]))
  x2 <- 0:(n[3] - 1) + 0.03 * n[3] * stats::rnorm(n[3])
  x3 <- 0:(n[3] - 1) + 0.03 * n[3] * stats::rnorm(n[3])
  x4 <- -(0:(n[3] - 1) + 0.03 * n[3] * stats::rnorm(n[3]))

  df3 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  df <- dplyr::bind_rows(df1, df2, df3)

  if (p > 4) {

    cli::cli_alert_info("Adding noise dimensions to reach the desired dimensionality.")

    noise_mat <- gen_noise_dims(
      n = NROW(df), num_noise = p - 4,
      min_n = -0.5, max_n = 0.5
    )
    colnames(noise_mat) <- paste0("x", 5:p)
    df <- dplyr::bind_cols(df, noise_mat)

  }

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}

#' Generate Four Different Long Clusters
#'
#' This function generates a dataset consisting of four different long clusters.
#'
#' @param n A numeric vector (default: c(200, 150, 300, 150)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing the four different long clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' four_diff_long_clusters <- gen_fourLongClusts(n = c(200, 150, 300, 150), p = 4)
gen_fourLongClusts <- function(n = c(200, 150, 300, 150), p = 4) {

  if (p < 4) {
    stop(cli::cli_alert_danger("p should be 4 or greater."))
  }

  if (length(n) != 4) {
    stop(cli::cli_alert_danger("n should contain exactly 4 values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  x1 <- 0:(n[1] - 1) + 0.03 * n[1] * stats::rnorm(n[1]) - 150
  x2 <- 0:(n[1] - 1) + 0.03 * n[1] * stats::rnorm(n[1])
  x3 <- 0:(n[1] - 1) + 0.03 * n[1] * stats::rnorm(n[1]) - 20
  x4 <- 0:(n[1] - 1) + 0.03 * n[1] * stats::rnorm(n[1]) - 85

  df1 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- 0:(n[2] - 1) + 0.03 * n[2] * stats::rnorm(n[2]) + n[2] / 5
  x2 <- 0:(n[2] - 1) + 0.03 * n[2] * stats::rnorm(n[2]) - n[2] / 5
  x3 <- 0:(n[2] - 1) + 0.03 * n[2] * stats::rnorm(n[2]) + n[2] / 5
  x4 <- 0:(n[2] - 1) + 0.03 * n[2] * stats::rnorm(n[2]) - n[2] / 5

  df2 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- 0:(n[3] - 1) + 0.03 * n[3] * stats::rnorm(n[3]) - 70
  x2 <- -(0:(n[3] - 1) + 0.03 * n[3] * stats::rnorm(n[3]))
  x3 <- 0:(n[3] - 1) + 0.03 * n[3] * stats::rnorm(n[3])
  x4 <- 0:(n[3] - 1) + 0.03 * n[3] * stats::rnorm(n[3]) - 85

  df3 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- 0:(n[4] - 1) + 0.03 * n[4] * stats::rnorm(n[4]) - 70
  x2 <- -(0:(n[4] - 1) + 0.03 * n[4] * stats::rnorm(n[4])) + 150
  x3 <- 0:(n[4] - 1) + 0.03 * n[4] * stats::rnorm(n[4])
  x4 <- 0:(n[4] - 1) + 0.03 * n[4] * stats::rnorm(n[4]) + 85

  df4 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  df <- dplyr::bind_rows(df1, df2, df3, df4)

  if (p > 4) {

    cli::cli_alert_info("Adding noise dimensions to reach the desired dimensionality.")

    noise_mat <- gen_noise_dims(
      n = NROW(df), num_noise = p - 4,
      min_n = -0.5, max_n = 0.5
    )
    colnames(noise_mat) <- paste0("x", 5:p)
    df <- dplyr::bind_cols(df, noise_mat)

  }

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}


#' Generate Three Linear Clusters
#'
#' This function generates data with three linear clusters.
#'
#' @param n A numeric vector (default: c(200, 300, 150)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#'
#' @return A data containing the three long clusters.
#' @export
#'
#' @examples
#'
#' # Generate three linear clusters with noise with custom parameters
#' set.seed(20240412)
#' data <- gen_threeLongClusts(n = c(200, 300, 150), p = 4)
gen_threeLongClusts <- function(n = c(200, 300, 150), p = 4) {

  if (p < 4) {
    stop(cli::cli_alert_danger("p should be 4 or greater."))
  }

  if (length(n) != 3) {
    stop(cli::cli_alert_danger("n should contain exactly 3 values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  x1 <- 0:(n[1] - 1) + 0.03 * n[1] * stats::rnorm(n[1]) + 100
  x2 <- 0:(n[1] - 1) + 0.03 * n[1] * stats::rnorm(n[1]) - 100
  x3 <- 0:(n[1] - 1) + 0.03 * n[1] * stats::rnorm(n[1]) - 100
  x4 <- 0:(n[1] - 1) + 0.03 * n[1] * stats::rnorm(n[1]) + 100

  df1 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- 0:(n[2] - 1) + 0.03 * n[2] * stats::rnorm(n[2]) + n[2] / 5
  x2 <- 0:(n[2] - 1) + 0.03 * n[2] * stats::rnorm(n[2]) - n[2] / 5
  x3 <- 0:(n[2] - 1) + 0.03 * n[2] * stats::rnorm(n[2]) + n[2] / 5
  x4 <- 0:(n[2] - 1) + 0.03 * n[2] * stats::rnorm(n[2]) - n[2] / 5

  df2 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- 0:(n[3] - 1) + 0.03 * n[3] * stats::rnorm(n[3]) - 10
  x2 <- 0:(n[3] - 1) + 0.03 * n[3] * stats::rnorm(n[3]) + 10
  x3 <- 0:(n[3] - 1) + 0.03 * n[3] * stats::rnorm(n[3]) - 10
  x4 <- 0:(n[3] - 1) + 0.03 * n[3] * stats::rnorm(n[3]) + 10

  df3 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  df <- dplyr::bind_rows(df1, df2, df3)

  if (p > 4) {

    cli::cli_alert_info("Adding noise dimensions to reach the desired dimensionality.")

    noise_mat <- gen_noise_dims(
      n = NROW(df), num_noise = p - 4,
      min_n = -0.5, max_n = 0.5
    )
    colnames(noise_mat) <- paste0("x", 5:p)
    df <- dplyr::bind_cols(df, noise_mat)

  }

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}
