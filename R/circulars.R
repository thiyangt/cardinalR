#' Generate Any number of Circle Clusters
#'
#' This function generates a dataset representing a structure with any number of circles.
#'
#' @param n A numeric vector (default: c(200, 500, 300)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param k A numeric value (default: 3) representing the number of clusters.
#' @return A data containing the any number of circle clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' circular_clusters_data <- gen_circles(n = c(200, 500, 300), p = 4, k = 3)
gen_circles <- function(n = c(200, 500, 300), p = 4, k = 3) {

  if (k < 2) {
    stop("k should be 2 or greater.")
  }

  if (p < 4) {
    stop("p should be 4 or greater.")
  }

  if (length(n) != k) {
    stop("n should contain exactly k values.")
  }

  if (any(n < 0)) {
    stop("Values in n should be positive.")
  }

  ## Generate scale factors for circles
  scale_factors_vec <- runif(k - 2, 0, 1)

  theta1 <- stats::runif(n[1], 0.0, 2 * pi)
  x1 <- cos(theta1) + stats::rnorm(n[1], 10, 0.03)
  x2 <- sin(theta1) + stats::rnorm(n[1], 10, 0.03)
  x3 <- rep(0, n[1]) + stats::rnorm(n[1], 10, 0.03)
  x4 <- rep(0, n[1]) - stats::rnorm(n[1], 10, 0.03)

  df1 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  x1 <- stats::rnorm(n[2], 10, 0.03)
  x2 <- stats::rnorm(n[2], 10, 0.03)
  x3 <- rep(0, n[2]) + stats::rnorm(n[2], 10, 0.03)
  x4 <- rep(0, n[2]) - stats::rnorm(n[2], 10, 0.03)

  df2 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3,
                        x4 = x4)

  df <- dplyr::bind_rows(df1, df2)

  if (k > 2) {
    for (i in 1:(k-2)) {

      theta <- stats::runif(n[i], 0.0, 2 * pi)

      x1 <- scale_factors_vec[i] * cos(theta) + stats::rnorm(n[i], 10, 0.03)
      x2 <- scale_factors_vec[i] * sin(theta) + stats::rnorm(n[i], 10, 0.03)
      x3 <- rep(0, n[i]) + stats::rnorm(n[i], 10, 0.03)
      x4 <- rep(0, n[i]) - stats::rnorm(n[i], 10, 0.03)

      df3 <- tibble::tibble(x1 = x1,
                            x2 = x2,
                            x3 = x3,
                            x4 = x4)

      df <- dplyr::bind_rows(df, df3)

    }
  }

  return(df)
}

#' Generate Three Cell Cycle Data
#'
#' This function generates a dataset representing a structure with three cell cycles.
#'
#' @param n A numeric vector (default: c(200, 500, 300)) representing the sample sizes.
#' @param p A numeric value (default: 3) representing the number of dimensions.
#' @return A data containing the three cell cycle data.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' cell_cycle_data <- gen_three_cell_cycle(n = c(200, 500, 300), p = 3)
gen_three_cell_cycle <- function(n = c(200, 500, 300), p = 3) {

  if (p < 3) {
    stop(cli::cli_alert_danger("p should be 3 or greater."))
  }

  if (length(n) != 3) {
    stop(cli::cli_alert_danger("n should contain exactly 3 values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }


  r1 <- 2
  r2 <- 1

  theta1 <- stats::runif(n[1], 0, 2 * pi)
  x1 <- rep(0, n[1])
  x2 <- r1 * cos(theta1)
  x3 <- r2 * sin(theta1)

  df1 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3)

  theta2 <- stats::runif(n[2], 0, 2 * pi)
  x1 <- r2 * cos(theta2)
  x2 <- rep(0, n[2])
  x3 <- r1 * sin(theta2)

  df2 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3)

  theta3 <- stats::runif(n[3], 0, 2 * pi)
  x1 <- r1 * cos(theta3)
  x2 <- r2 * sin(theta3)
  x3 <- rep(0, n[3])

  df3 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3)

  df <- dplyr::bind_rows(df1, df2, df3)

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

#' Generate Three Curvy Cell Cycle Data
#'
#' This function generates a dataset representing a structure with three curvy cell cycles,
#'
#' @param n A numeric vector (default: c(200, 500, 300)) representing the sample sizes.
#' @param p A numeric value (default: 3) representing the number of dimensions.
#' @return A data containing the three curvy cell cycles.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' curvy_cell_cycle_data <- gen_three_curvy_cycle(n = c(200, 500, 300), p = 3)
gen_three_curvy_cycle <- function(n = c(200, 500, 300), p = 3) {

  if (p < 3) {
    stop(cli::cli_alert_danger("p should be 3 or greater."))
  }

  if (length(n) != 3) {
    stop(cli::cli_alert_danger("n should contain exactly 3 values."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }


  r <- sqrt(3) / 3

  theta1 <- stats::runif(n[1], 0, 2 * pi)
  x1 <- cos(theta1)
  x2 <- r + sin(theta1)
  x3 <- cos(3 * theta1) / 3

  df1 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3)

  theta2 <- stats::runif(n[2], 0, 2 * pi)
  x1 <- cos(theta2) + 0.5
  x2 <- sin(theta2) - r / 2
  x3 <- cos(3 * theta2) / 3

  df2 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3)

  theta3 <- stats::runif(n[3], 0, 2 * pi)
  x1 <- cos(theta3) - 0.5
  x2 <- sin(theta3) - r / 2
  x3 <- cos(3 * theta3) / 3

  df3 <- tibble::tibble(x1 = x1,
                        x2 = x2,
                        x3 = x3)

  df <- dplyr::bind_rows(df1, df2, df3)

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


extend_circle <- function(n, d) {
  if (length(n) != 1) {
    stop("n should be a single integer specifying the number of points along the 'circle'")
  }
  if (d < 2) {
    stop("d must be at least 2 for a circular structure")
  }

  theta1 <- (0:(n[1] - 1)) * (2 * pi / n[1])
  coords <- matrix(0, nrow = n[1], ncol = d)

  # First two dimensions (essential for the base circle)
  # coords[, 1] <- cos(theta1)
  # coords[, 2] <- sin(theta1)

  cs <- cos(.4)
  sn <- sin(.4)
  coords[, 1] <- cos(theta1)
  coords[, 2] <- cs * sin(theta1)
  coords[, 3] <- -sn * sin(theta1)

  # Introduce scaling factors for subsequent dimensions
  scaling_factors <- sqrt(cumprod(c(1, rep(0.5, d - 3)))) # Example: decreasing scale

  # Add remaining dimensions with sinusoidal patterns
  for (i in 4:d) {
    # Introduce a phase shift for each dimension to make them distinct
    phase_shift <- (i - 2) * (pi / (2 * d))
    coords[, i] <- scaling_factors[i-2] * sin(theta1 + phase_shift)
  }

  return(coords)
}
