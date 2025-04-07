#' Generate Circle in p-d
#'
#' This function generates a dataset representing a structure with a circle.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param r1 A numeric value (default: 1) representing the radius scale factor along x1.
#' @param r2 A numeric value (default: 1) representing the radius scale factor along x2.
#' @return A data containing a circle.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' circle_data <- gen_circle_pd(n = 500, p = 4)
gen_circle_pd <- function(n = 500, p = 3, r1 = 1, r2 = 1){

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (length(n) != 1) {
    cli::cli_abort("n should be a single integer specifying the number of points.")
  }

  theta <- stats::runif(n, 0.0, 2 * pi)
  coords <- matrix(0, nrow = n, ncol = p)
  coords[, 1] <- r1 * cos(theta) #+ stats::rnorm(n, 10, 0.03)
  coords[, 2] <- r2 * sin(theta) #+ stats::rnorm(n, 10, 0.03)

  # Introduce scaling factors for subsequent dimensions
  scaling_factors <- sqrt(cumprod(c(1, rep(0.5, p - 2)))) # Example: decreasing scale

  if (p > 2) {
    # Apply remaining dimensions with sinusoidal patterns
    for (i in 3:p) {
      # Introduce a phase shift for each dimension to make them distinct
      phase_shift <- (i - 2) * (pi / (2 * p))
      coords[, i] <- scaling_factors[i-1] * sin(theta + phase_shift)
    }
  }

  df <- suppressMessages(tibble::as_tibble(coords, .name_repair = "unique"))
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}

#' Generate Shited Any number of Circle Clusters
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
#' circular_clusters_data <- gen_shifted_clusts_circle(n = c(200, 500, 300), p = 4, k = 3)
gen_shifted_clusts_circle <- function(n = c(200, 500, 300), p = 4, k = 3) {

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

  ## Generate scale factors for circles
  scale_factors_vec <- runif(k, 0, 2)

  df <- tibble::tibble()

  for (i in 1:k) {

    df3 <- gen_circle_pd(n[i], p = p, r1 = scale_factors_vec[i], r2 = scale_factors_vec[i]) |>
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
#' cell_cycle_data <- gen_overlapped_clusts_circle(n = c(200, 500, 300), p = 4, k = 3)
gen_overlapped_clusts_circle <- function(n = c(200, 500, 300), p = 4, k = 3) {

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

    generated_tibble <- gen_circle_pd(n[i], p = p, r1 = r1, r2 = r2)

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

#' Generate Curvy Cell Cycle in p-d
#'
#' This function generates a dataset representing a structure with a curvy cell cycle.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param r A numeric value (default: sqrt(3) / 3) representing the radius factor along x2.
#' @return A data containing a curvy cell cycle.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' curvy_cycle_data <- gen_curvy_cycle_pd(n = 500, p = 4, shift = c(0, sqrt(3) / 3, 0), scale_fac = c(1, 1, 1/3))
gen_curvy_cycle_pd <- function(n = 500, p = 4, shift = c(0, sqrt(3) / 3, 0), scale_fac = c(1, 1, 1/3)){

  if (p <= 3) {
    cli::cli_abort("p should be greater than 3.")
  }

  if (length(n) != 1) {
    cli::cli_abort("n should be a single integer specifying the number of points.")
  }

  theta <- stats::runif(n, 0.0, 2 * pi)
  coords <- matrix(0, nrow = n, ncol = p)
  coords[, 1] <- scale_fac[1] * (shift[1] + cos(theta))
  coords[, 2] <- scale_fac[2] * (shift[2] + sin(theta))
  coords[, 3] <- scale_fac[3] * (shift[3] + cos(3 * theta))

  # Introduce scaling factors for subsequent dimensions
  scaling_factors <- sqrt(cumprod(c(1, rep(0.5, p - 3)))) # Example: decreasing scale

  if (p > 3) {
    # Apply remaining dimensions with sinusoidal patterns
    for (i in 4:p) {
      # Introduce a phase shift for each dimension to make them distinct
      phase_shift <- (i - 2) * (pi / (2 * p))
      coords[, i] <- scaling_factors[i-2] * sin(theta + phase_shift)
    }
  }

  df <- suppressMessages(tibble::as_tibble(coords, .name_repair = "unique"))
  names(df) <- paste0("x", 1:p)

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
#' curvy_cell_cycle_data <- gen_overlapped_clusts_curvy_cycle(n = c(200, 500, 300), p = 4, k = 3)
gen_overlapped_clusts_curvy_cycle <- function(n = c(200, 500, 300), p = 4, k = 3) {

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

  ## Generate shift factors for circles
  shift_vec <- sample(seq(-1, 1, 0.2), k)

  df <- tibble::tibble()

  for (i in 1:k) {

    shift_vec <- sample(seq(-0.5, 0.5, 0.2), 3)
    scale_vec <- sample(seq(-0.5, 0.5, 0.2), 3)

    df3 <- gen_curvy_cycle_pd(n[i], p = p, shift = shift_vec, scale_fac = c(1, 1, 1/3)) |>
      dplyr::mutate(cluster = paste0("cluster", i))

    df <- dplyr::bind_rows(df, df3)

  }

  ## To swap rows
  df <- randomize_rows(df)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)

}

#' Generate Circular in p-d
#'
#' This function generates a dataset representing a structure with a circular.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param r1 A numeric value (default: 1) representing the radius scale factor along x2.
#' @param r2 A numeric value (default: 1) representing the radius scale factor along x3.
#' @return A data containing a circular.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' circular_data <- gen_circular_pd(n = 500, p = 4)
gen_circular_pd <- function(n = 500, p = 4, r = cos(.4), r2 = sin(.4), shift = 0){

  if (p <= 3) {
    cli::cli_abort("p should be greater than 3.")
  }

  if (length(n) != 1) {
    cli::cli_abort("n should be a single integer specifying the number of points.")
  }

  theta <- (0:(n[1] - 1)) * (2 * pi / n[1])
  x1 <- cos(theta)
  x2 <- r1 * sin(theta)
  x3 <- -r2 * sin(theta)

  df1 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3)


  theta <- (0:(n - 1)) * (2 * pi / n)
  coords <- matrix(0, nrow = n, ncol = p)
  coords[, 1] <- shift + cos(theta)
  coords[, 2] <- r1 * sin(theta1)
  coords[, 3] <- r2 * sin(theta1)

  # Introduce scaling factors for subsequent dimensions
  scaling_factors <- sqrt(cumprod(c(1, rep(0.5, p - 3)))) # Example: decreasing scale

  if (p > 3) {
    # Apply remaining dimensions with sinusoidal patterns
    for (i in 4:p) {
      # Introduce a phase shift for each dimension to make them distinct
      phase_shift <- (i - 2) * (pi / (2 * p))
      coords[, i] <- scaling_factors[i-2] * sin(theta + phase_shift)
    }
  }

  df <- suppressMessages(tibble::as_tibble(coords, .name_repair = "unique"))
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")

  return(df)
}

#' Generate Two Link Circular Clusters
#'
#' This function generates a dataset representing a structure with two link circular clusters.
#'
#' @param n A numeric vector (default: c(200, 300)) representing the sample sizes.
#' @param p A numeric value (default: 3) representing the number of dimensions.
#'
#' @return A data containing the generated two linked circular clusters.
#' @export
#'
#' @examples
#'
#' # Generate linked data with noise with custom parameters
#' set.seed(20240412)
#' data <- gen_two_circulars(n = c(200, 300), p = 3)
gen_two_circulars <- function(n = c(200, 300), p = 3) {

  if (p < 3) {
    stop(cli::cli_alert_danger("p should be 3 or greater."))
  }

  if (length(n) != 2) {
    stop(cli::cli_alert_danger("n should contain exactly 2 values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  cs <- cos(.4)
  sn <- sin(.4)

  theta1 <- (0:(n[1] - 1)) * (2 * pi / n[1])
  x1 <- cos(theta1)
  x2 <- cs * sin(theta1)
  x3 <- -sn * sin(theta1)

  df1 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3)

  theta2 <- (0:(n[2] - 1)) * (2 * pi / n[2])
  x1 <- 1 + cos(theta2)
  x2 <- sn * sin(theta2)
  x3 <- cs * sin(theta2)

  df2 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3)

  df <- dplyr::bind_rows(df1, df2)

  if (p > 3) {

    cli::cli_alert_info("Adding noise dimensions to reach the desired dimensionality.")

    noise_mat <- gen_noise_dims(
      n = NROW(df), num_noise = p - 3,
      min_n = -0.5, max_n = 0.5
    )
    colnames(noise_mat) <- paste0("x", 4:p)
    df <- dplyr::bind_cols(df, noise_mat)

  }

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}
