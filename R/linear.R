#' Generate points on a plane in high-dimensions
#'
#' This function generates points on a plane in high-dimensions.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#'
#' @return A data containing the generated points on the plane.
#'
#' @examples
#' set.seed(20240412)
#' plane_points <- gen_hyperplane(n = 500, p = 4)
#'
#' @export
gen_hyperplane <- function(n = 500, p = 4) {

  if (p < 2) {
    stop(cli::cli_alert_danger("p should be 2 or greater."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  # Generate n-1 random dimensions
  x <- matrix(runif(n * (p - 1), -1, 1), ncol = (p - 1))

  # Compute the last coordinate to satisfy the plane equation
  coeff <- rep(3, p)
  intercept <- 0
  x_last <- (intercept - x %*% coeff[1:(p- 1)]) / coeff[p]

  # Combine all coordinates
  plane_points <- tibble::as_tibble(cbind(x, x_last)) |>
    set_names(paste0("x", 1:p))

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(plane_points)

}

#' Generate Hyper Plane with Hole
#'
#' This function generates a dataset representing a hyper plane with a hole.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#'
#' @return A data containing the hyperplane data with a hole.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' plane_hole_data <- gen_hyperplane_with_hole(n = 1000, p = 4)
gen_hyperplane_with_hole <- function(n = 500, p = 4) {

  if (p < 2) {
    stop(cli::cli_alert_danger("p should be 2 or greater."))
  }

  if (any(n < 0)) {
    stop(cli::cli_alert_danger("Values in n should be positive."))
  }

  plane_points <- gen_hyperplane(n = n, p = p)

  # Compute Euclidean distance from the center
  distances <- sqrt(rowSums(plane_points^2))

  # Remove points inside the hole
  hole_radius <- 1
  plane_points <- plane_points |>
    dplyr::filter(distances > hole_radius)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(plane_points)

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
#' long_cluster <- gen_two_long_clusts(n = c(200, 300), p = 4)
gen_two_long_clusts <- function(n = c(200, 300), p = 4) {

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
#' three_diff_linear <- gen_three_angled_long_clusts(n = c(200, 300, 150), p = 4)
gen_three_angled_long_clusts <- function(n = c(200, 300, 150), p = 4) {

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
#' four_diff_long_clusters <- gen_four_long_clusts(n = c(200, 150, 300, 150), p = 4)
gen_four_long_clusts <- function(n = c(200, 150, 300, 150), p = 4) {

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
#' data <- gen_three_long_clusts(n = c(200, 300, 150), p = 4)
gen_three_long_clusts <- function(n = c(200, 300, 150), p = 4) {

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
